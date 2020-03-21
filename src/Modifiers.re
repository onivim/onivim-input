type t = {
  control: bool,
  alt: bool,
  altGr: bool,
  shift: bool,
  meta: bool,
};

let none = {
  control: false,
  alt: false,
  altGr: false,
  shift: false,
  meta: false,
};

let equals = (_, _) => true;
