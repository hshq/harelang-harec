
export def EPERM: int           = 1;       // Operation not permitted
export def ENOENT: int          = 2;       // No such file or directory
export def ESRCH: int           = 3;       // No such process
export def EINTR: int           = 4;       // Interrupted system call
export def EIO: int             = 5;       // Input/output error
export def ENXIO: int           = 6;       // Device not configured
export def E2BIG: int           = 7;       // Argument list too long
export def ENOEXEC: int         = 8;       // Exec format error
export def EBADF: int           = 9;       // Bad file descriptor
export def ECHILD: int          = 10;      // No child processes
export def EDEADLK: int         = 11;      // Resource deadlock avoided
            // 11 was EAGAIN
export def ENOMEM: int          = 12;      // Cannot allocate memory
export def EACCES: int          = 13;      // Permission denied
export def EFAULT: int          = 14;      // Bad address
export def ENOTBLK: int         = 15;      // Block device required
export def EBUSY: int           = 16;      // Device / Resource busy
export def EEXIST: int          = 17;      // File exists
export def EXDEV: int           = 18;      // Cross-device link
export def ENODEV: int          = 19;      // Operation not supported by device
export def ENOTDIR: int         = 20;      // Not a directory
export def EISDIR: int          = 21;      // Is a directory
export def EINVAL: int          = 22;      // Invalid argument
export def ENFILE: int          = 23;      // Too many open files in system
export def EMFILE: int          = 24;      // Too many open files
export def ENOTTY: int          = 25;      // Inappropriate ioctl for device
export def ETXTBSY: int         = 26;      // Text file busy
export def EFBIG: int           = 27;      // File too large
export def ENOSPC: int          = 28;      // No space left on device
export def ESPIPE: int          = 29;      // Illegal seek
export def EROFS: int           = 30;      // Read-only file system
export def EMLINK: int          = 31;      // Too many links
export def EPIPE: int           = 32;      // Broken pipe

// math software
export def EDOM: int            = 33;      // Numerical argument out of domain
export def ERANGE: int          = 34;      // Result too large

// non-blocking and interrupt i/o
export def EAGAIN: int          = 35;      // Resource temporarily unavailable
export def EWOULDBLOCK: int     = EAGAIN;  // Operation would block
export def EINPROGRESS: int     = 36;      // Operation now in progress
export def EALREADY: int        = 37;      // Operation already in progress

// ipc/network software -- argument errors
export def ENOTSOCK: int        = 38;      // Socket operation on non-socket
export def EDESTADDRREQ: int    = 39;      // Destination address required
export def EMSGSIZE: int        = 40;      // Message too long
export def EPROTOTYPE: int      = 41;      // Protocol wrong type for socket
export def ENOPROTOOPT: int     = 42;      // Protocol not available
export def EPROTONOSUPPORT: int = 43;      // Protocol not supported
export def ESOCKTNOSUPPORT: int = 44;      // Socket type not supported
export def ENOTSUP: int         = 45;      // Operation not supported
// export def EOPNOTSUPP: int      = ENOTSUP; // Operation not supported on socket

export def EPFNOSUPPORT: int    = 46;      // Protocol family not supported
export def EAFNOSUPPORT: int    = 47;      // Address family not supported by protocol family
export def EADDRINUSE: int      = 48;      // Address already in use
export def EADDRNOTAVAIL: int   = 49;      // Can't assign requested address

// ipc/network software -- operational errors
export def ENETDOWN: int        = 50;      // Network is down
export def ENETUNREACH: int     = 51;      // Network is unreachable
export def ENETRESET: int       = 52;      // Network dropped connection on reset
export def ECONNABORTED: int    = 53;      // Software caused connection abort
export def ECONNRESET: int      = 54;      // Connection reset by peer
export def ENOBUFS: int         = 55;      // No buffer space available
export def EISCONN: int         = 56;      // Socket is already connected
export def ENOTCONN: int        = 57;      // Socket is not connected
export def ESHUTDOWN: int       = 58;      // Can't send after socket shutdown
export def ETOOMANYREFS: int    = 59;      // Too many references: can't splice
export def ETIMEDOUT: int       = 60;      // Operation timed out
export def ECONNREFUSED: int    = 61;      // Connection refused

export def ELOOP: int           = 62;      // Too many levels of symbolic links
export def ENAMETOOLONG: int    = 63;      // File name too long

// should be rearranged
export def EHOSTDOWN: int       = 64;      // Host is down
export def EHOSTUNREACH: int    = 65;      // No route to host
export def ENOTEMPTY: int       = 66;      // Directory not empty

// quotas & mush
export def EPROCLIM: int        = 67;      // Too many processes
export def EUSERS: int          = 68;      // Too many users
export def EDQUOT: int          = 69;      // Disc quota exceeded

// Network File System
export def ESTALE: int          = 70;      // Stale NFS file handle
export def EREMOTE: int         = 71;      // Too many levels of remote in path
export def EBADRPC: int         = 72;      // RPC struct is bad
export def ERPCMISMATCH: int    = 73;      // RPC version wrong
export def EPROGUNAVAIL: int    = 74;      // RPC prog. not avail
export def EPROGMISMATCH: int   = 75;      // Program version wrong
export def EPROCUNAVAIL: int    = 76;      // Bad procedure for program

export def ENOLCK: int          = 77;      // No locks available
export def ENOSYS: int          = 78;      // Function not implemented

export def EFTYPE: int          = 79;      // Inappropriate file type or format
export def EAUTH: int           = 80;      // Authentication error
export def ENEEDAUTH: int       = 81;      // Need authenticator

// Intelligent device errors
export def EPWROFF: int         = 82;      // Device power is off
export def EDEVERR: int         = 83;      // Device error, e.g. paper out

export def EOVERFLOW: int       = 84;      // Value too large to be stored in data type

// Program loading errors
export def EBADEXEC: int        = 85;      // Bad executable
export def EBADARCH: int        = 86;      // Bad CPU type in executable
export def ESHLIBVERS: int      = 87;      // Shared library version mismatch
export def EBADMACHO: int       = 88;      // Malformed Macho file

export def ECANCELED: int       = 89;      // Operation canceled

export def EIDRM: int           = 90;      // Identifier removed
export def ENOMSG: int          = 91;      // No message of desired type
export def EILSEQ: int          = 92;      // Illegal byte sequence
export def ENOATTR: int         = 93;      // Attribute not found

export def EBADMSG: int         = 94;      // Bad message
export def EMULTIHOP: int       = 95;      // Reserved
export def ENODATA: int         = 96;      // No message available on STREAM
export def ENOLINK: int         = 97;      // Reserved
export def ENOSR: int           = 98;      // No STREAM resources
export def ENOSTR: int          = 99;      // Not a STREAM
export def EPROTO: int          = 100;     // Protocol error
export def ETIME: int           = 101;     // STREAM ioctl timeout

export def EOPNOTSUPP: int      = 102;     // Operation not supported on socket

export def ENOPOLICY: int       = 103;     // No such policy registered

export def ENOTRECOVERABLE: int = 104;     // State not recoverable
export def EOWNERDEAD: int      = 105;     // Previous owner died

export def EQFULL: int          = 106;     // Interface output queue is full
export def ELAST: int           = 106;     // Must be equal largest errno
