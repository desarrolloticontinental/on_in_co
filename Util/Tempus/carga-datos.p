DEF VAR pOptions AS CHAR.
DEF VAR pArchivo AS CHAR.
DEF VAR lOptions AS CHAR.

DEF TEMP-TABLE Detalle LIKE TempusPers.

RUN c:\newsie\on_in_co\aplic\lib\tt-file-to-text (OUTPUT pOptions, OUTPUT pArchivo).
IF pOptions = "" THEN RETURN.

DELETE FROM TempusPers.
FOR EACH pl-flg-mes NO-LOCK WHERE codcia = 1
    AND periodo = 2012
    AND nromes = 09,
    FIRST pl-pers NO-LOCK WHERE pl-pers.codper = pl-flg-mes.codper:
    CREATE TempusPers.
    ASSIGN
        TempusPers.Apellido_materno = pl-pers.matper
        TempusPers.Apellido_paterno = pl-pers.patper
        TempusPers.Cargo = pl-flg-mes.cargos
        TempusPers.Centro_de_costo = pl-flg-mes.ccosto
        TempusPers.Codigo = pl-flg-mes.codper
        TempusPers.DNI = pl-pers.nrodocid
        TempusPers.Empresa = '01'
        TempusPers.Fecha_de_cese = pl-flg-mes.vcontr
        TempusPers.Fecha_de_ingreso = pl-flg-mes.fecing
        TempusPers.Fecha_de_nacimiento = pl-pers.fecnac
        TempusPers.Nombres = pl-pers.nomper
        TempusPers.Seccion = pl-flg-mes.seccion.
    CREATE Detalle.
    BUFFER-COPY TempusPers TO Detalle.
END.

FIND FIRST Detalle NO-LOCK NO-ERROR.
IF NOT AVAILABLE Detalle THEN DO:
    MESSAGE 'No hay datos que imprimir' VIEW-AS ALERT-BOX WARNING.
    RETURN.
END.

RUN c:\newsie\on_in_co\aplic\lib\tt-file (TEMP-TABLE Detalle:HANDLE, pArchivo, pOptions).
