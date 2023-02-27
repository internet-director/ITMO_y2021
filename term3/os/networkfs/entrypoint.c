#include "header.h"
#include "http.h"

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Lezhen Stanislav");
MODULE_VERSION("0.01");

void fix_string(const char *str, char *result, int len) {
  int i = 0;
  while (i < len) {
    result[i] = '%';
    sprintf(result + i + 1, "%02x", str[i / 3]);
    i += 3;
  }
}

int code_f(struct inode *parent_inode, struct dentry *child_dentry,
           const char *type, int flag) {
  ino_t root;
  struct inode *inode;
  const char *name = child_dentry->d_name.name;
  root = parent_inode->i_ino;
  ino_t http_ans = 0;
  char my_ino[11];
  snprintf(my_ino, 11, "%d", root);
  char enc_str[256 * 3 + 1];
  fix_string(name, enc_str, strlen(name) * 3);
  if (networkfs_http_call(token, "create", (ino_t)&http_ans, sizeof(ino_t), 3,
                          "parent", my_ino, "name", enc_str, "type", type)) {
    return -1;
  }
  inode = networkfs_get_inode(parent_inode->i_sb, NULL, flag, http_ans);
  inode->i_op = &networkfs_inode_ops;
  inode->i_fop = NULL;
  d_add(child_dentry, inode);
  return 0;
}

int empty_call(struct inode *parent_inode, struct dentry *child_dentry,
               const char *command) {
  const char *name = child_dentry->d_name.name;
  ino_t root;
  root = parent_inode->i_ino;
  ino_t http_ans = 0;
  char my_ino[11];
  snprintf(my_ino, 11, "%d", root);
  char enc_str[256 * 3 + 1];
  fix_string(name, enc_str, strlen(name) * 3);

  if (networkfs_http_call(token, command, (ino_t)&http_ans, sizeof(ino_t), 2,
                          "parent", my_ino, "name", enc_str)) {
    return -1;
  }
  return 0;
}

struct dentry *networkfs_mount(struct file_system_type *fs_type, int flags,
                               const char *tkn, void *data) {
  for (int i = 0; i < 37; i++) {
    token[i] = tkn[i];
  }
  struct dentry *ret = mount_nodev(fs_type, flags, data, networkfs_fill_super);
  if (ret == NULL) {
    printk(KERN_ERR "Can't mount file system");
  } else {
    printk(KERN_INFO "Mounted successfuly");
  }
  return ret;
}

int networkfs_fill_super(struct super_block *sb, void *data, int silent) {
  struct inode *inode;
  inode = networkfs_get_inode(sb, NULL, S_IFDIR, 1000);
  sb->s_root = d_make_root(inode);
  if (sb->s_root == NULL) {
    return -ENOMEM;
  }
  printk(KERN_INFO "return 0\n");
  return 0;
}

struct inode *networkfs_get_inode(struct super_block *sb,
                                  const struct inode *dir, umode_t mode,
                                  int i_ino) {
  struct inode *inode;
  inode = new_inode(sb);
  if (inode != NULL) {
    inode_init_owner(sb->s_user_ns, inode, dir, mode);
  }
  inode->i_op = &networkfs_inode_ops;
  inode->i_fop = &networkfs_dir_ops;
  inode->i_ino = i_ino;
  inode->i_mode |= 511;
  return inode;
};

int networkfs_iterate(struct file *filp, struct dir_context *ctx) {
  char fsname[10];
  struct dentry *dentry;
  struct inode *inode;
  unsigned long offset;
  int stored;
  unsigned char ftype;
  ino_t ino;
  ino_t dino;
  dentry = filp->f_path.dentry;
  inode = dentry->d_inode;
  offset = filp->f_pos;
  stored = 0;
  ino = inode->i_ino;
  entry_data http_ans = {};
  char my_ino[11];
  snprintf(my_ino, 11, "%d", ino);
  if (networkfs_http_call(token, "list", (void *)&http_ans, sizeof(entry_data),
                          1, "inode", my_ino)) {
    return -1;
  }

  if (offset >= http_ans.entries_count) {
    return 0;
  }
  strcpy(fsname, ".");
  ftype = DT_DIR;
  dino = ino;
  offset += 1;
  ctx->pos += 1;
  dir_emit(ctx, ".", 1, dino, ftype);

  strcpy(fsname, "..");
  ftype = DT_DIR;
  dino = dentry->d_parent->d_inode->i_ino;
  offset += 1;
  ctx->pos += 1;
  dir_emit(ctx, "..", 2, dino, ftype);
  for (int i = 0; i < http_ans.entries_count; i++) {
    printk("%s\n", http_ans.entries[i].name);
    offset++;
    dir_emit(ctx, http_ans.entries[i].name, strlen(http_ans.entries[i].name),
             http_ans.entries[i].ino, http_ans.entries[i].entry_type);
  }
  ctx->pos = offset;
  return http_ans.entries_count;
}

struct dentry *networkfs_lookup(struct inode *parent_inode,
                                struct dentry *child_dentry,
                                unsigned int flag) {
  ino_t root;
  struct inode *inode;
  const char *name = child_dentry->d_name.name;
  root = parent_inode->i_ino;
  entry_datat http_ans = {};
  char my_ino[11];
  snprintf(my_ino, 11, "%d", root);
  char enc_str[256 * 3 + 1];
  fix_string(name, enc_str, strlen(name) * 3);
  if (networkfs_http_call(token, "lookup", (void *)&http_ans,
                          sizeof(entry_datat), 2, "parent", my_ino, "name",
                          enc_str)) {
    return NULL;
  }
  inode = networkfs_get_inode(parent_inode->i_sb, NULL,
                              http_ans.entry_type == DT_REG ? S_IFREG : S_IFDIR,
                              http_ans.ino);
  d_add(child_dentry, inode);
  return NULL;
}

int networkfs_create(struct user_namespace *fs_namespace,
                     struct inode *parent_inode, struct dentry *child_dentry,
                     umode_t mode, bool b) {
  return code_f(parent_inode, child_dentry, "file", S_IFREG | S_IRWXUGO);
}

int networkfs_mkdir(struct user_namespace *fs_name, struct inode *parent_inode,
                    struct dentry *child_dentry, umode_t mode) {
  return code_f(parent_inode, child_dentry, "directory", S_IFDIR | S_IRWXUGO);
}

int networkfs_unlink(struct inode *parent_inode, struct dentry *child_dentry) {
  return empty_call(parent_inode, child_dentry, "unlink");
}

int networkfs_rmdir(struct inode *parent_inode, struct dentry *child_dentry) {
  return empty_call(parent_inode, child_dentry, "rmdir");
}

int networkfs_link(struct dentry *old_dentry, struct inode *parent_dir,
                   struct dentry *new_dentry) {
  int errcode = 0;
  const char parent_ino[11], source_ino[11];
  snprintf(parent_ino, 11, "%d", parent_dir->i_ino);
  snprintf(source_ino, 11, "%d", old_dentry->d_inode->i_ino);
  return networkfs_http_call(token, "link", (int)&errcode, sizeof(int), 3,
                             "source", source_ino, "parent", parent_ino, "name",
                             new_dentry->d_name.name);
}

ssize_t networkfs_read(struct file *filp, char *buffer, size_t len,
                       loff_t *offset) {
  if (len < 0) return -1;
  return copy_from_user(filp + *offset, buffer, len);
}

ssize_t networkfs_write(struct file *filp, const char *buffer, size_t len,
                        loff_t *offset) {
  return 0;
  return copy_from_user(filp + *offset, buffer, len);
}

void networkfs_kill_sb(struct super_block *sb) {
  printk(KERN_INFO
         "networkfs super block is destroyed. Unmount successfully.\n");
}

int networkfs_init(void) {
  if (register_filesystem(&networkfs_fs_type)) {
    printk(KERN_INFO "Cant init networkfs!\n");
    return 1;
  }
  printk(KERN_INFO "Hello, World!\n");
  return 0;
}

void networkfs_exit(void) {
  unregister_filesystem(&networkfs_fs_type);
  printk(KERN_INFO "Goodbye!\n");
}

module_init(networkfs_init);
module_exit(networkfs_exit);
