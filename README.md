# serialcomms
Units to locate serial hardware devices, intended to support the serial.pp unit.

This tries to hide messy parsing of the /sys tree, but is likely to be at the mercy of Linux kernel module writers.

Also has support for detecting device hotplugging.
