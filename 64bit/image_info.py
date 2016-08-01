import sys
import numpy as np
import struct
import logging

logging.basicConfig(level=logging.DEBUG)
logger = logging.getLogger("gipsy.dataset")

filename = sys.argv[1]
filename_desc = filename.replace(".image", ".descr")
"""
typedef struct {
   fint   version;        /* different versions are incompatible */
   fint   subversion;     /* sub-versions of a version are compatible */
                          /* they are defined in ???.h and set in ...? */
   fint   one;            /* the number 1  to determine byte order */
   fint   osdep;          /* os dependent info */
/* --- this point in the structure must align to a multiple of 8 bytes --- */
   double ax_origin[MAXDIM];   /* axis origins */
   fint   ax_size  [MAXDIM];   /* axis sizes */
   fint8   ax_factor[MAXDIM+1]; /* factors */
   fint   naxis;          /* number of axes */
   fint   nitems;         /* number of descriptor items (from version 2.2) */
   fint   reserved2;      /* not currently used */
   fint   reserved3;      /* not currently used */
   fint   rec_start;      /* first data record */
   fint   n_alloc;        /* number of records allocated */
   fint   maxrec;         /* current maximum number of records in file */
   fint   n_buck;         /* size of hash table */
   fint   spare_fint[8];  /* spare elements */
   fint   free;           /* free list pointer */
   fint   hash_tab[1];    /* hash table (only first element declared) */
} _header, *header; /* ----------------------------------------- file header */

"""

class GipsyDataset(object):
  def __init__(self, name):
    name = name.replace(".image", "").replace(".descr", "")
    self.path_image = name + ".image"
    self.path_descr = name + ".descr"
    logger.info("opening descriptor: %s", self.path_descr)
    
    with open(self.path_descr) as f:
      data_descr = f.read()
      fmt = 'iiii20d20i21q2i6i8i2i'
      logger.debug("header is %s bytes", struct.calcsize(fmt))
      numbers = struct.unpack(fmt, data_descr[:struct.calcsize(fmt)])
      self.version, self.subversion = numbers[0:2];  numbers = numbers[4:]
      ax_origin = numbers[:20]; numbers = numbers[20:]
      ax_size = numbers[:20]; numbers = numbers[20:]
      ax_factor = numbers[:21]; numbers = numbers[21:]
      self.naxis, self.ndescriptors= numbers[0:2]; numbers = numbers[2:]
      #numbers = numbers[2:] # skip reserved
      #numbers = numbers[20:]; 
      self.ax_origin = ax_origin[:self.naxis]
      self.ax_size = ax_size[:self.naxis]
      self.ax_factor = ax_factor[:self.naxis]

      logger.info("ax_origin: %s", self.ax_origin)
      logger.info("ax_size: %s", self.ax_size)
      logger.info("ax_factor: %s", self.ax_factor)
      
      logger.info("naxis: %s", self.naxis)
      logger.info("ndescriptors: %s", self.ndescriptors)
      logger.info("rest: %s", numbers)
    #print numbers[4:]; print numbers[4:][20:]; print numbers[4:][40:]
    
    logger.info("memory mapping image: %s", self.path_image)
    data = np.memmap(self.path_image, dtype=np.float32)
    self.data = data.reshape((self.ax_size))
    
  def info(self):
    imin = np.unravel_index(np.argmin(self.data), self.data.shape)
    imax = np.unravel_index(np.argmax(self.data), self.data.shape)
    logger.info("minimum at: %s, with value: %s", imin, self.data.__getitem__(imin))
    logger.info("maximim at: %s, with value: %s", imax, self.data.__getitem__(imax))
  

ds = GipsyDataset(sys.argv[1])
ds.info()
#data = np.memmap(filename, dtype=np.float32)
#print data.shape

