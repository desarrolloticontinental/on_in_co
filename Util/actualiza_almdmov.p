

define temp-table wrk_dmov
    fields wrk_codalm like almcmov.codalm
    fields wrk_tipmov like almcmov.tipmov
    fields wkr_codmov like almcmov.codmov
    fields wkr_nroser like almcmov.nroser
    fields wrk_nrodoc like almcmov.nrodoc
    fields wrk_codmat like almdmov.codmat
    fields wrk_fchdoc like almdmov.fchdoc.

input from value("d:\sie\ajustes.csv").
repeat:
    create wrk_dmov.
    wkr_codmov = 50.
    import DELIMITER ","
        wrk_codalm
        wrk_tipmov
        wkr_nroser
        wrk_nrodoc
        wrk_codmat
        wrk_fchdoc.
end.
input close.

loop:
DO TRANSACTION ON ERROR UNDO loop, RETRY loop:

FOR EACH wrk_dmov where wrk_codalm <> "" BREAK BY
    wrk_codalm BY
    wrk_tipmov BY
    wkr_codmov BY
    wkr_nroser BY
    wrk_nrodoc BY
    wrk_codmat BY
    wrk_fchdoc:
    IF FIRST-OF(wrk_nrodoc) THEN DO:
        FIND Almcmov WHERE
            Almcmov.CodCia = 1 AND
            Almcmov.CodAlm = wrk_codalm AND
            Almcmov.TipMov = wrk_tipmov AND
            Almcmov.CodMov = wkr_codmov AND
            Almcmov.NroSer = wkr_nroser AND
            Almcmov.NroDoc = wrk_nrodoc NO-ERROR.
        IF AVAILABLE Almcmov THEN Almcmov.fchdoc = wrk_fchdoc.
        else do:
            message
                "No existe registro detalle"
                Almcmov.NroDoc
                view-as alert-box.
            undo loop, leave loop.
        end.
    END.
    DISPLAY
        wrk_codalm
        wrk_tipmov
        wkr_codmov
        wkr_nroser
        wrk_nrodoc
        wrk_codmat
        wrk_fchdoc
        .
    FIND Almdmov WHERE
        Almdmov.CodCia = 1 AND
        Almdmov.CodAlm = wrk_codalm AND
        Almdmov.TipMov = wrk_tipmov AND
        Almdmov.CodMov = wkr_codmov AND
        Almdmov.NroSer = wkr_nroser AND
        Almdmov.NroDoc = wrk_nrodoc AND
        Almdmov.CodMat = wrk_codmat no-error.
    IF AVAILABLE Almdmov THEN Almdmov.fchdoc = wrk_fchdoc.
    else do:
        message
            "No existe registro detalle" skip
        wrk_codalm skip
        wrk_tipmov skip
        wkr_codmov skip
        wkr_nroser skip
        wrk_nrodoc skip
        wrk_codmat skip
        wrk_fchdoc
            view-as alert-box.
        undo loop, leave loop.
    end.
    
END.
MESSAGE "Deshace?" view-as alert-box question
buttons yes-no update RPTA as logical.
if rpta then undo loop, leave loop.
END.

