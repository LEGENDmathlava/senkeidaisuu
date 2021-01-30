use std::convert::From;
use std::error;
use std::fmt;

pub struct Error {
    repr: Repr,
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.repr, f)
    }
}

enum Repr {
    Simple(ErrorKind),
    Custom(Box<Custom>),
}

#[derive(Debug)]
struct Custom {
    kind: ErrorKind,
    error: Box<dyn error::Error + Send + Sync>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ErrorKind {
    Io,
    NoRows,
    NoCols,
    MissMatchLenOfRows,
    IndexOutOfBounds,
    InfinityOrNan,
    MissmMatchSizeOfMatrix,
    SingularMatrix,
}

impl ErrorKind {
    pub(crate) fn as_str(&self) -> &'static str {
        match *self {
            ErrorKind::Io => "IO error",
            ErrorKind::NoRows => "expected one or more rows",
            ErrorKind::NoCols => "expected on ore more columns",
            ErrorKind::MissMatchLenOfRows => "mismatch len of rows",
            ErrorKind::IndexOutOfBounds => "index out of bounds",
            ErrorKind::InfinityOrNan => "element cannot be Infinity or NaN",
            ErrorKind::MissmMatchSizeOfMatrix => "mismatch size of matrix",
            ErrorKind::SingularMatrix => "not a regular matrix",
        }
    }
}

impl From<ErrorKind> for Error {
    fn from(kind: ErrorKind) -> Error {
        Error {
            repr: Repr::Simple(kind),
        }
    }
}

impl Error {
    pub fn new<E>(kind: ErrorKind, error: E) -> Error
    where
        E: Into<Box<dyn error::Error + Send + Sync>>,
    {
        Self::_new(kind, error.into())
    }

    fn _new(kind: ErrorKind, error: Box<dyn error::Error + Send + Sync>) -> Error {
        Error {
            repr: Repr::Custom(Box::new(Custom { kind, error })),
        }
    }

    pub fn kind(&self) -> ErrorKind {
        match self.repr {
            Repr::Custom(ref c) => c.kind,
            Repr::Simple(kind) => kind,
        }
    }
}

impl fmt::Debug for Repr {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Repr::Custom(ref c) => fmt::Debug::fmt(&c, fmt),
            Repr::Simple(kind) => fmt.debug_tuple("Kind").field(&kind).finish(),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.repr {
            Repr::Custom(ref c) => c.error.fmt(fmt),
            Repr::Simple(kind) => write!(fmt, "{}", kind.as_str()),
        }
    }
}

impl error::Error for Error {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match self.repr {
            Repr::Simple(..) => None,
            Repr::Custom(ref c) => c.error.source(),
        }
    }
}
