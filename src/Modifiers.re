type t = {
  control: bool,
  alt: bool,
  shift: bool,
  meta: bool,
};

let create = (~control, ~alt, ~shift, ~meta) => {
  control,
  alt,
  shift,
  meta,
};

let none = {control: false, alt: false, shift: false, meta: false};

let equals = (_, _) => true;
