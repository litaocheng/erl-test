#! /bin/bash
if !(sysctl -w "net.ipv4.tcp_mem= 94500000 915000000 927000000");then
    echo "change the sysctl error"
    exit 1
fi
