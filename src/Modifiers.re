type t = {
  control: bool,
  alt: bool,
  altGr: bool,
  shift: bool,
  meta: bool,
};

let create =
    (~control=false, ~alt=false, ~altGr=false, ~shift=false, ~meta=false, ()) => {
  control,
  alt,
  altGr,
  shift,
  meta,
};

let none = {
  control: false,
  alt: false,
  altGr: false,
  shift: false,
  meta: false,
};

let equals = (_, _) => true;
