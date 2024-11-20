
DELETE FROM expogrupo.
DELETE FROM expogrupodetail.
DELETE FROM expogrupoean.

DEF VAR x-linea AS CHAR NO-UNDO.

INPUT FROM d:\tmp\grupo.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    CREATE ExpoGrupo.
    ASSIGN
        ExpoGrupo.CodCia = 001
        ExpoGrupo.Descripcion = SUBSTRING(x-linea,6)
        ExpoGrupo.FlgEst = 'A'
        ExpoGrupo.Grupo = SUBSTRING(x-linea,1,5).
    CREATE ExpoGrupoEan.
    ASSIGN
        ExpoGrupoEan.CodCia = ExpoGrupo.CodCia 
        ExpoGrupoEan.Grupo = ExpoGrupo.Grupo
        ExpoGrupoEan.EAN = ExpoGrupo.Grupo.
END.
INPUT CLOSE.


INPUT FROM d:\tmp\detalle.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    CREATE ExpoGrupoDetail.
    ASSIGN
        ExpoGrupoDetail.CodCia = 001
        ExpoGrupoDetail.CodMat = SUBSTRING(x-linea,11,6)
        ExpoGrupoDetail.Descripcion = SUBSTRING(x-linea,21)
        ExpoGrupoDetail.Grupo = SUBSTRING(x-linea,1,5)
        ExpoGrupoDetail.Secuencia = INTEGER(SUBSTRING(x-linea,6,5)).
END.
INPUT CLOSE.
