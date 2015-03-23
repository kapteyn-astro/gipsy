#if !defined(_addpgdrv_h_)
#define _addpgdrv_h_
extern int addpgdrv(void(*driver)(), 
                     char *name, char *hardcopy,
                     int *width, int *height, int *hoffset, int *voffset);
#endif /* _addpgdrv_h_ */
