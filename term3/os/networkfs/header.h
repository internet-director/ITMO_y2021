//
// Created by stasan on 21.12.22.
//

#ifndef OS_2022_NETWORKFS_INTERNET_DIRECTOR_HEADER_H
#define OS_2022_NETWORKFS_INTERNET_DIRECTOR_HEADER_H
#include <linux/fs.h>
#include <linux/init.h>
#include <linux/kernel.h>
#include <linux/module.h>

struct dentry *networkfs_mount(struct file_system_type *, int, const char *,
                               void *);
void networkfs_kill_sb(struct super_block *);
struct inode *networkfs_get_inode(struct super_block *, const struct inode *,
                                  umode_t, int);
int networkfs_fill_super(struct super_block *, void *, int);
struct dentry *networkfs_lookup(struct inode *, struct dentry *, unsigned int);
int networkfs_iterate(struct file *, struct dir_context *);

struct dentry *mount_nodev(struct file_system_type *, int, void *,
                           int (*fill_super)(struct super_block *, void *,
                                             int));

int networkfs_unlink(struct inode *, struct dentry *);
int networkfs_create(struct user_namespace *, struct inode *, struct dentry *,
                     umode_t, bool);
int networkfs_mkdir(struct user_namespace *, struct inode *, struct dentry *,
                    umode_t);
int networkfs_rmdir(struct inode *, struct dentry *);

ssize_t networkfs_read(struct file *filp, char *buffer, size_t len,
                       loff_t *offset);
ssize_t networkfs_write(struct file *filp, const char *buffer, size_t len,
                        loff_t *offset);
int networkfs_link(struct dentry *, struct inode *, struct dentry *);

char token[37];

typedef struct _entry_data {
  size_t entries_count;
  struct entry {
    unsigned char entry_type;  // DT_DIR (4) or DT_REG (8)
    ino_t ino;
    char name[256];
  } entries[16];
} entry_data;

typedef struct _entry_datat {
  unsigned char entry_type;  // DT_DIR (4) or DT_REG (8)
  ino_t ino;
} entry_datat;

struct file_system_type networkfs_fs_type = {.name = "networkfs",
                                             .mount = networkfs_mount,
                                             .kill_sb = networkfs_kill_sb};

struct inode_operations networkfs_inode_ops = {
    .lookup = networkfs_lookup,
    .create = networkfs_create,
    .unlink = networkfs_unlink,
    .mkdir = networkfs_mkdir,
    .rmdir = networkfs_rmdir,
    .link = networkfs_link,
};

struct file_operations networkfs_dir_ops = {
    .iterate = networkfs_iterate,
    .read = networkfs_read,
    .write = networkfs_write,
};

#endif  // OS_2022_NETWORKFS_INTERNET_DIRECTOR_HEADER_H
