#if !defined(_gdsd_basic_h_)
#define _gdsd_basic_h_
#include "gipsyc.h"
#include "gdsparams.h"
extern void gds_handle_c(fchar funval, fchar set, fint *err);
extern void gds_create_c (fchar set, fint *err);
extern void gds_close_c(fchar set, fint *err);
extern void gds_closeall_c(void);
extern fint gds_rhed( fchar set, gds_coord **cinfo);
extern fint gds_whed( fchar set, gds_coord *cinfo);
extern fint gds_frhed( fchar set);
extern fint gds_ftype_c( fchar set, fint *err );
extern fint gds_itype_c( fchar set, fint *err );
extern fint gds_prime_c(fchar set, fint *err);
extern fint gds_nitems_c(fchar set, fint *err);
extern fint gds_rename_c( fchar old,fchar new );
extern bool gds_exist_c ( fchar set, fint *err );
extern void gds_delete_c (fchar set, fint *err);
extern void gds_optimize_c (fchar set, fint *err);
extern void gds_sync_c (fchar set, fint *err);
extern fint gds_recover_c (fchar set);

extern void gdsd_read_c(fchar set, fchar key, fint *level, char buf[],
                        fint *nb, fint *pos, fint *done, fint *err);
extern void gdsd_write_c(fchar set, fchar key, fint *level, char buf[],
                         fint *nb, fint *pos, fint *done, fint *err);
void gdsd_delete_c (fchar set, fchar key, fint *level, fint *err);
extern void gdsd_delall_c(fchar set, fchar key, fint *err);
void gdsd_find_c(fchar funval, fchar set, fint *level,
                 fint *record, fint *err);
fint gdsd_length_c (fchar set, fchar key, fint *level, fint *err);
void gdsd_rewind_c(fchar set, fchar key, fint *level, fint *err);
extern void gds_lock_c( fchar set, fint *err);
extern void gds_unlock_c( fchar set, fint *err);
extern int gds___image( fchar name, void (*proc)() );
extern int gds___fail(int verify, fint erri, fint *erro);
extern int gds___char2str(fchar c, char *s, int ls);
extern int gds___str2char(char *s, fchar c);
#endif
