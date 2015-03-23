#if !defined (_radiogroup_h_)
#define _radiogroup_h_
ident RadioGroup(char *key);
void RadioAddKey(ident gr, char *key, char *value);
void RadioDeleteGroup(ident *gr);
#endif
