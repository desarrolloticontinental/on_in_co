
DEFINE VARIABLE x-nromes AS INTEGER NO-UNDO.
DEFINE VARIABLE x-periodo AS INTEGER NO-UNDO.
DEFINE BUFFER B-FLG-MES FOR PL-FLG-MES.
DEFINE BUFFER b-DHABIENTE FOR PL-DHABIENTE.

x-periodo = 2008.
x-nromes = 2.

FOR EACH PL-FLG-MES WHERE
    PL-FLG-MES.CodCia = 1 AND
    PL-FLG-MES.Periodo = 2008 AND
    PL-FLG-MES.NroMes = 1 NO-LOCK:

    IF PL-FLG-MES.SitAct = "Inactivo" THEN NEXT.

    DISPLAY PL-FLG-MES.CodPer.

    FIND b-FLG-MES WHERE
        b-FLG-MES.CodCia = PL-FLG-MES.CodCia AND
        b-FLG-MES.Periodo = PL-FLG-MES.Periodo AND
        b-FLG-MES.NroMes = x-nromes AND
        b-FLG-MES.CodPln = PL-FLG-MES.CodPln AND
        b-FLG-MES.CodPer = PL-FLG-MES.CodPer NO-ERROR.

    IF AVAILABLE B-FLG-MES THEN DO:
        ASSIGN
            B-FLG-MES.AfilEPS = PL-FLG-MES.AfilEPS
            B-FLG-MES.Categoria = PL-FLG-MES.Categoria
            B-FLG-MES.CodEPS = PL-FLG-MES.CodEPS
            B-FLG-MES.Discapaci = PL-FLG-MES.Discapaci
            B-FLG-MES.IndRenta = PL-FLG-MES.IndRenta
            B-FLG-MES.Jornada[1] = PL-FLG-MES.Jornada[1]
            B-FLG-MES.Jornada[2] = PL-FLG-MES.Jornada[2]
            B-FLG-MES.Jornada[3] = PL-FLG-MES.Jornada[3]
            B-FLG-MES.ModFormat = PL-FLG-MES.ModFormat
            B-FLG-MES.MotivoFin = PL-FLG-MES.MotivoFin
            B-FLG-MES.NivEducat = PL-FLG-MES.NivEducat
            B-FLG-MES.Ocupacion = PL-FLG-MES.Ocupacion
            B-FLG-MES.PeriodIng = PL-FLG-MES.PeriodIng
            B-FLG-MES.QtaCatego = PL-FLG-MES.QtaCatego
            B-FLG-MES.RegLabora = PL-FLG-MES.RegLabora
            B-FLG-MES.RegPensio = PL-FLG-MES.RegPensio
            B-FLG-MES.SitEspeci = PL-FLG-MES.SitEspeci
            B-FLG-MES.Situacion = PL-FLG-MES.Situacion
            B-FLG-MES.TpoConTra = PL-FLG-MES.TpoConTra
            B-FLG-MES.TpoPago = PL-FLG-MES.TpoPago
            B-FLG-MES.TpoTrabaj = PL-FLG-MES.TpoTrabaj.
    END.            
    
    RELEASE b-FLG-MES.

    /* Derechohabientes */
    FOR EACH PL-DHABIENTE WHERE
        PL-DHABIENTE.CodCia = PL-FLG-MES.CodCia AND
        PL-DHABIENTE.Periodo = PL-FLG-MES.Periodo AND
        PL-DHABIENTE.CodPln = PL-FLG-MES.CodPln AND
        PL-DHABIENTE.NroMes = PL-FLG-MES.NroMes AND
        PL-DHABIENTE.CodPer = PL-FLG-MES.CodPer NO-LOCK:
        IF PL-DHABIENTE.SitDerHab = "11" THEN NEXT. /* Baja */
        FIND b-DHABIENTE WHERE
            b-DHABIENTE.CodCia = PL-DHABIENTE.CodCia AND
            b-DHABIENTE.Periodo = x-periodo AND
            b-DHABIENTE.NroMes = x-nromes AND
            b-DHABIENTE.CodPln = PL-DHABIENTE.CodPln AND
            b-DHABIENTE.CodPer = PL-DHABIENTE.CodPer AND
            b-DHABIENTE.TpoDocId = PL-DHABIENTE.TpoDocId AND
            b-DHABIENTE.NroDocId = PL-DHABIENTE.NroDocId NO-ERROR.
        IF NOT AVAILABLE b-DHABIENTE THEN CREATE b-DHABIENTE.
        BUFFER-COPY PL-DHABIENTE TO b-DHABIENTE
        ASSIGN
            b-DHABIENTE.Periodo = x-periodo
            b-DHABIENTE.NroMes = x-nromes.
    END.

END.
