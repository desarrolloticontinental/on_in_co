def var csecc as char format "x(40)".
for each pl-flg-mes where
    codcia = 1 and
    periodo = 2008:
    if not can-find(first pl-secc where
        pl-secc.seccion = pl-flg-mes.seccion) then do:
        csecc = "".
        if pl-flg-mes.seccion begins "00" then do:
            find pl-secc where
                pl-secc.seccion begins pl-flg-mes.seccion
                no-lock no-error.
            if available pl-secc then
                csecc = pl-secc.seccion.
        end.
        else do:
            find pl-secc where
                substring(pl-secc.seccion,6) begins pl-flg-mes.seccion
                no-lock no-error.
            if available pl-secc then
                csecc = pl-secc.seccion.
        end.
        /*
        if csecc <> "" then do:
            assign pl-flg-mes.seccion = csecc.
            */
            display
                codper
                pl-flg-mes.seccion
                csecc
                with stream-io width 132.
                /*
        end.
        */
    end.
end.
