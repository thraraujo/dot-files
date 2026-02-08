#!/bin/bash

BLUE='\033[0;34m'
PURPLE='\033[0;35m'
RED='\033[0;31m'
RESET='\033[0m' # Reset text color to default

# Here I update the list of devices connected to my network 
sudo nmap -sP 192.168.15.0/24 > /dev/null

# Here I save the connected devices to this file for later manipulation
arp > /home/thiago/.config/scripts/04-who-network/connected

# Here I have the mac address of the connected devices. 
# awk 'NR > 1 {print $3}' connected  

LOOP_COUNT=$(wc -l connected | awk '{print $1}')

for ((n=2; n <= LOOP_COUNT ; n++)) ; do
    mac_add=$(awk "NR==$n {print \$3}" connected) 
    tes_len=$(grep "$mac_add" devices)

    if [ -z "$tes_len" ] ; then 
        echo -e "!! Device ${RED}$mac_add${RESET} is not known"
    else 
        device=$(awk "NR==$n {print \$1}" devices)
        echo -e ">> ${PURPLE}$device${RESET} is connected"
    fi 
done

# cat /home/thiago/.config/scripts/04-who-network/connected
