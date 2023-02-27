#ifndef NETWORKFS_HTTP
#define NETWORKFS_HTTP

#include <linux/types.h>

#define ESOCKNOCREATE 0x2001
#define ESOCKNOCONNECT 0x2002
#define ESOCKNOMSGSEND 0x2003
#define ESOCKNOMSGRECV 0x2004
#define EHTTPBADCODE 0x2005
#define EHTTPMALFORMED 0x2006
#define EPROTMALFORMED 0x2007

/**
 * networkfs_http_call - make a call to networkfs API.
 * @token:           Unique filesystem token.
 * @method:          API method name, e.g. "fs.list".
 * @response_buffer: Pointer to memory space for writing the response.
 *                   There should be available at least @buffer_size bytes.
 * @arg_size:        Number of arguments provided.
 * @...:             Exactly twice of @arg_size string arguments in format
 *                   key1, value1, key2, value2, ...
 *
 * This method makes an HTTP call to networkfs API server and parses the result.
 *
 * Return:
 * * If HTTP session succeeds, returns `result->status`.
 *   `result->response` is written into @response_buffer.
 * * Otherwise, returns negated errno, either defined in `errno-base.h`
 *   or in `http.h`, and @response_buffer stays unaltered.
 */
int64_t networkfs_http_call(const char *token, const char *method,
                            char *response_buffer, size_t buffer_size,
                            size_t arg_size, ...);

#endif