#include "http.h"

#include <linux/inet.h>

const char *HTTP_REQUEST_LINE = "GET /teaching/os/networkfs/v1/";
const char *HTTP_REQUEST_HEADERS =
    " HTTP/1.1\r\nHost:nerc.itmo.ru\r\nConnection: close\r\n\r\n";
const char *SERVER_IP = "77.234.215.132";
const char *HTTP_LENGTH_HEADER = "Content-Length: ";

// callee should call free_request on received buffer
int fill_request(struct kvec *vec, const char *token, const char *method,
                 size_t arg_size, va_list args) {
  // 2048 bytes for URL and 64 bytes for anything else
  char *request_buffer = kzalloc(2048 + 64, GFP_KERNEL);
  if (request_buffer == 0) {
    return -ENOMEM;
  }

  strcpy(request_buffer, HTTP_REQUEST_LINE);
  strcat(request_buffer, token);
  strcat(request_buffer, "/fs/");
  strcat(request_buffer, method);

  for (int i = 0; i < arg_size; i++) {
    strcat(request_buffer, i == 0 ? "?" : "&");
    strcat(request_buffer, va_arg(args, char *));
    strcat(request_buffer, "=");
    strcat(request_buffer, va_arg(args, char *));
  }

  strcat(request_buffer, HTTP_REQUEST_HEADERS);

  memset(vec, 0, sizeof(struct kvec));
  vec->iov_base = request_buffer;
  vec->iov_len = strlen(request_buffer);

  return 0;
}

int receive_all(struct socket *sock, char *buffer, size_t buffer_size) {
  struct msghdr hdr;
  struct kvec vec;

  int read = 0;

  while (read < buffer_size) {
    memset(&hdr, 0, sizeof(struct msghdr));
    memset(&vec, 0, sizeof(struct kvec));
    vec.iov_base = buffer + read;
    vec.iov_len = buffer_size - read;
    int ret = kernel_recvmsg(sock, &hdr, &vec, 1, vec.iov_len, 0);
    if (ret == 0) {
      break;
    } else if (ret < 0) {
      return ESOCKNOMSGRECV;
    }
    read += ret;
  }

  return read;
}

int64_t parse_http_response(char *raw_response, size_t raw_response_size,
                            char *response, size_t response_size) {
  char *buffer = raw_response;

  // Read Response Line
  {
    char *status_line = strsep(&buffer, "\r");
    strsep(&status_line, " ");
    if (status_line == 0) {
      return -EHTTPMALFORMED;
    }
    char *status_code = strsep(&status_line, " ");
    if (strcmp(status_code, "200") != 0) {
      return -EHTTPBADCODE;
    }
  }

  int length = -1;

  while (true) {
    if (buffer == 0) {
      return -EHTTPMALFORMED;
    }
    char *header = strsep(&buffer, "\r");
    ++header;  // skip \n
    if (strcmp(header, "") == 0) {
      // end of headers
      break;
    }

    if (strncmp(header, HTTP_LENGTH_HEADER, strlen(HTTP_LENGTH_HEADER)) == 0) {
      int error = kstrtoint(header + strlen(HTTP_LENGTH_HEADER), 0, &length);
      if (error != 0) {
        return -EHTTPMALFORMED;
      }
    }
  }
  ++buffer;  // skip last '\n'

  if (length == -1) {
    return -EHTTPMALFORMED;
  }

  if (buffer + length > raw_response + raw_response_size) {
    return -EHTTPMALFORMED;
  }

  if (length < sizeof(int64_t)) {
    return -EPROTMALFORMED;
  }

  length -= sizeof(int64_t);

  if (length > response_size) {
    return -ENOSPC;
  }

  int64_t return_value;
  memcpy(&return_value, buffer, sizeof(int64_t));

  buffer += sizeof(int64_t);
  memcpy(response, buffer, length);

  return return_value;
}

int64_t networkfs_http_call(const char *token, const char *method,
                            char *response_buffer, size_t buffer_size,
                            size_t arg_size, ...) {
  struct socket *sock;
  int64_t error;

  error = sock_create_kern(&init_net, AF_INET, SOCK_STREAM, IPPROTO_TCP, &sock);
  if (error < 0) {
    return -ESOCKNOCREATE;
  }

  struct sockaddr_in s_addr = {.sin_family = AF_INET,
                               .sin_addr = {.s_addr = in_aton(SERVER_IP)},
                               .sin_port = htons(80)};

  error = kernel_connect(sock, (struct sockaddr *)&s_addr,
                         sizeof(struct sockaddr_in), 0);
  if (error != 0) {
    sock_release(sock);
    return -ESOCKNOCONNECT;
  }

  struct kvec kvec;
  va_list args;
  va_start(args, arg_size);
  error = fill_request(&kvec, token, method, arg_size, args);
  va_end(args);

  if (error != 0) {
    kernel_sock_shutdown(sock, SHUT_RDWR);
    sock_release(sock);
    return error;
  }

  struct msghdr msg;
  memset(&msg, 0, sizeof(struct msghdr));

  error = kernel_sendmsg(sock, &msg, &kvec, 1, kvec.iov_len);
  kfree(kvec.iov_base);

  if (error < 0) {
    kernel_sock_shutdown(sock, SHUT_RDWR);
    sock_release(sock);
    return -ESOCKNOMSGSEND;
  }

  size_t raw_buffer_size = buffer_size + 1024;  // add 1KB for HTTP headers
  char *raw_response_buffer = kmalloc(raw_buffer_size, GFP_KERNEL);
  if (raw_response_buffer == 0) {
    kernel_sock_shutdown(sock, SHUT_RDWR);
    sock_release(sock);
    return -ENOMEM;
  }
  int read_bytes = receive_all(sock, raw_response_buffer, raw_buffer_size);

  kernel_sock_shutdown(sock, SHUT_RDWR);
  sock_release(sock);

  if (read_bytes < 0) {
    kfree(raw_response_buffer);
    return -ESOCKNOMSGRECV;
  }

  error = parse_http_response(raw_response_buffer, read_bytes, response_buffer,
                              buffer_size);

  kfree(raw_response_buffer);
  return error;
}
