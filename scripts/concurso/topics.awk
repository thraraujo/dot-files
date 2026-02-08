#!/usr/bin/awk -f

BEGIN {
    FS = "|"

    FILE = ARGV[1]
    CONTROL_FLOW = ARGV[2]

    # Remove the non-filename argument from ARGV to prevent AWK from processing it as a file
    delete ARGV[2]
}

# Main processing block
{
    if (CONTROL_FLOW == "1") {
        if ($0 ~ /UFTM/) {
            print $3, $2
        }
    } else if (CONTROL_FLOW == "2") {
        if ($0 ~ /UFF-VR/) {
            print $4, $2
        }
    } else if (CONTROL_FLOW == "3") {
        if ($0 ~ /UFF-NITEROI/) {
            print $5, $2
        }
    } else if (CONTROL_FLOW == "4") {
        if ($0 ~ /UNIFEI/) {
            print $6, $2
        }
    }
}

