import sys
import numpy as np
import struct
import logging
import collections

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


typedef struct {
   fint	key_ind;        /* key record indicator */
   fint	length;	        /* length of descriptor item in bytes */
   fint	readpos;        /* number of last byte read (start at 1) */
   fint	level;          /* coordinate level */
   fint	next_key;       /* index of next key record */
   fint	next_ext;       /* index of next extension record */
   fint	last_ext;       /* index of last extension record */
   fint	curr_ext;       /* index of last extension record read */
   char	type;           /* type of data (C, I, L, F, D or \0) */
   char	name[KEY_LEN];  /* name of descriptor item */
} _keyhead, *keyhead; /* ------------------------- header part of key record */

#define SKH    (sizeof(_keyhead))
#define SEH    (sizeof(_exthead))
#define SL     (sizeof(fint))
#define KEY_AL (SL+(SL*SKH-SKH)%SL)     /* alignment in key records         */
#define EXT_AL (SL+(SL*SEH-SEH)%SL)     /* alignment in extension records   */
#define KEY_DL (REC_SIZ-SKH-KEY_AL )    /* data length of key record        */
#define EXT_DL (REC_SIZ-SEH-EXT_AL )    /* data length of extension record  */


"""
REC_SIZ =  200 #                  /* internal record size (bytes)       */
GDS_KEYLEN  =   21          #  /* descriptor key length (bytes)      *
SKH = 8*4 + 1 + GDS_KEYLEN
SL = 4
KEY_AL = (SL+(SL*SKH-SKH)%SL)

class GipsyDataset(object):
  def __init__(self, name):
    name = name.replace(".image", "").replace(".descr", "")
    self.path_image = name + ".image"
    self.path_descr = name + ".descr"
    logger.info("opening descriptor: %s", self.path_descr)
    self.headers = collections.OrderedDict()

    with open(self.path_descr) as f:
		data_all = data_descr = f.read()
		self.descriptor_size = len(data_descr)
		logger.info("descriptor is %s bytes (0x%x)", len(data_descr), len(data_descr))
		fmt = 'iiii20d20i21q2i6i8i2i'
		logger.debug("header is %s bytes", struct.calcsize(fmt))
		numbers = struct.unpack(fmt, data_descr[:struct.calcsize(fmt)])
		data_descr = data_descr[struct.calcsize(fmt):]
		self.version, self.subversion = numbers[0:2];  numbers = numbers[4:]
		ax_origin = numbers[:20]; numbers = numbers[20:]
		ax_size = numbers[:20]; numbers = numbers[20:]
		ax_factor = numbers[:21]; numbers = numbers[21:]
		self.naxis, self.ndescriptors= numbers[0:2]; numbers = numbers[2:]
		numbers = numbers[2:] # reserved
		self.rec_start, self.n_alloc, self.maxrec, self.n_buck = numbers[0:4]; numbers = numbers[4:]

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
		logger.info("rec_start: %s", self.rec_start)
		logger.info("n_alloc: %s", self.n_alloc)
		logger.info("maxrec: %s", self.maxrec)
		logger.info("n_buck: %s", self.n_buck)
		logger.info("rest: %s", numbers)
		indices = []
		for k in range(self.n_buck):
			fmt = 'i'
			numbers = struct.unpack(fmt, data_descr[:struct.calcsize(fmt)])
			data_descr = data_descr[struct.calcsize(fmt):]
			index = numbers[0]
			if index:
				offset = REC_SIZ*index
				logger.info("buck: %s offset: %x", index, offset)
				record_data = data_all[offset:]

				fmt = '8ic%is' % GDS_KEYLEN
				data = struct.unpack(fmt, record_data[:struct.calcsize(fmt)])
				key_ind, length, readpos, level, next_key, next_ext, last_ext, curr_ext, type, name = data
				data = record_data[struct.calcsize(fmt)+KEY_AL:struct.calcsize(fmt)+KEY_AL+length]
				indices.append(index)
				logger.info("\tname: %s level: %s type: %r length: %s", name, level, type, length)
				logger.info("\tnext_key: %s next_ext: %s last_ext: %s curr_ext: %s", next_key, next_ext, last_ext, curr_ext)
				logger.info("\t%r" % data)
				assert curr_ext == index, "corrupt file?"
				self.headers[name] = data
			#logger.info(" left over: %s", len(data_descr))
			#while k:
			#	data_all
		indices = list(sorted(indices))
		logger.info("used indices: %r", indices)
		for name, value in self.headers.items():
			print("%s = %r" % (name, value))

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
