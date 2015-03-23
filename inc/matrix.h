#if !defined(_matrix_h_)
#define _matrix_h_
double **dmatrix( int xlo, int ylo, int xhi, int yhi );
void freedmatrix( double **m, int xlo, int ylo );
float **fmatrix( int xlo, int ylo, int xhi, int yhi );
void freefmatrix( float **m, int xlo, int ylo );
int **imatrix( int xlo, int ylo, int xhi, int yhi );
void freeimatrix( int **m, int xlo, int ylo );
#endif
