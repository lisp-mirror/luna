#!/bin/sh

## presets
preset_red='000 bc0000 d21755 e92daa f4f';
preset_green='000 217867 2ca05a abc837 fd5';
preset_pink='000 a34480 b85493 d163a8 dc87bc';


lunagen() {
  background="$1";
  stripe1="$2";
  stripe2="$3";
  stripe3="$4";
  stripe4="$5";
  
  printf '%s\n' \
    '<?xml version="1.0" encoding="UTF-8"?>';
  printf '%s' \
    '<svg version="1.1" viewBox="0 0 1024 1024" xmlns="http://www.w3.org/2000/svg">' \
      '<defs><clipPath id="a"><path d="m7351 800.93a340 340 0 0 1 123 261.59 340 340 0 0 1-340 340 340 340 0 0 1-261.5-123.15 400 400 0 0 0 389.5 311.15 400 400 0 0 0 400-400 400 400 0 0 0-311-389.59z" fill="#0ff"/></clipPath></defs>'\
      '<g transform="translate(0 -98.52)"><g transform="matrix(.92773 0 0 .92773 -6225.2 -493.97)">' \
        '<circle cx="7262" cy="1190.5" r="512" fill="#'"$background"'"/>' \
        '<g clip-path="url(#a)">' \
          '<circle cx="7134" cy="1062.5" r="600" clip-path="none" fill="#'"$stripe4"'"/>' \
          '<circle cx="7134" cy="1062.5" r="520" clip-path="none" fill="#'"$stripe3"'"/>' \
          '<circle cx="7134" cy="1062.5" r="460" clip-path="none" fill="#'"$stripe2"'"/>' \
          '<circle cx="7134" cy="1062.5" r="400" clip-path="none" fill="#'"$stripe1"'"/>' \
        '</g>' \
      '</g></g>' \
    '</svg>';
  printf '\n';
}


iscolor() { case "$1" in ([0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]) return 0;; ([0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]) return 0;; esac; return 1; }
arecolors() { while test "$#" -gt 0; do if ! iscolor "$1"; then return 1; fi; shift; done; return 0; }

if test "$#" -eq 5 && arecolors "$@"; then
  lunagen "$@";
  exit 0;
fi

if test "$#" -eq 2 -a x"$1" = x'--preset' && test x"$2" = x'red' -o x"$2" = x'green' -o x"$2" = x'pink'; then
  eval 'lunagen $preset_'"$2";
  exit 0;
fi


binname="$(basename "$0")";
printf 'Usage: %s BACKGROUND STRIPE1 STRIPE2 STRIPE3 STRIPE4\n' "$binname";
printf '       %s --preset {red,green,pink}\n\n' "$binname";

printf '  Colors are formatted as hexadecimal `RGB` and `RRGGBB`.\n';
printf '  Stripes are numbered inside to outside.\n';
exit 1;
