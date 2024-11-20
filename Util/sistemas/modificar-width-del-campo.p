&SCOPED-DEFINE Tabla pf-g004
&SCOPED-DEFINE Campo seguridad

FIND first _file where _file._file-name = "{&Tabla}".
find first _field of _file where _field._field-name = "{&Campo}".
update _field._width.
