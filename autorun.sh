function compile() {
    # make clean
    clear
    pkill perkc
    make debug_run
}

compile&

(inotifywait -m -e close_write $(find . -type f \( -name "*.ml" -o -name "*.mli" -o -name "*.perk" -o -name "*.mly" \)) Makefile |
    while read file_path file_event file_name
    do
        compile&
    done)