def var copia as int.
def var rpta as logical.

SYSTEM-DIALOG PRINTER-SETUP NUM-COPIES copia UPDATE Rpta.
DISPLAY copia rpta.
