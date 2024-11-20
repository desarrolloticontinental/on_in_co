/* Corrige POSITION de una tabla */

DEFINE VARIABLE iCount AS INTEGER INITIAL 2   NO-UNDO.
DEFINE VARIABLE cTable AS CHARACTER  NO-UNDO LABEL "Table".

UPDATE cTable.

FIND _file WHERE _file._file-name = cTable.

FOR EACH _field WHERE _field._file-recid = RECID(_file)
     BY _field._field-rpos:
   ASSIGN _field._field-rpos = iCount
          iCount = iCount + 1.
END.
