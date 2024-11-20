FOR EACH vtactrkped WHERE codcia = 1
    AND coddoc = 'PED'
    AND flgsit = 'P'
    AND codubic = 'EFAC'
    AND DATE(fechai) < TODAY
    BY fechai DESC:
    /* buscamos las hojas de ruta */
    FOR EACH vtadtrkped OF vtactrkped NO-LOCK WHERE vtadtrkped.codubic = 'EFAC'
        AND vtadtrkped.flgsit <> 'A':
        FIND FIRST di-rutad WHERE di-rutad.codcia = 001
            AND di-rutad.coddoc = 'H/R'
            AND di-rutad.codref = vtadtrkped.codref
            AND di-rutad.nroref = vtadtrkped.nroref
            AND DI-Rutad.FlgEst = 'C'
            NO-LOCK NO-ERROR.
        IF AVAILABLE di-rutad THEN DO:
            vtactrkped.flgsit = 'C'.
            DISPLAY
                vtactrkped.coddiv
                vtactrkped.coddoc
                vtactrkped.nroped
                vtactrkped.fechai
                vtactrkped.codubic
                vtadtrkped.codref
                vtadtrkped.nroref
                di-rutad.nrodoc
                WITH STREAM-IO NO-BOX WIDTH 320.
            PAUSE 0.
        END.
    END.

END.
