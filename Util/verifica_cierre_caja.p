        DEFINE VARIABLE amtnac AS DECIMAL NO-UNDO.
        DEFINE VARIABLE amtusa AS DECIMAL NO-UNDO.

        /* INGRESOS */
        output to d:\ahorapues.txt NO-CONVERT.

            PUT UNFORMATTED 
                "Division|"
                "Fecha|"
                "Cod|"
                "Recibo|"
                "Tipo|"
                "Usuario|"
                "Cliente|"
                "Nombre|"
                "FlgEst|"
                "FlgCie|"
                "Fecha Cierre|"
                "Vuelto S/.|"
                "S/. 1|"
                "S/. 2|"
                "S/. 3|"
                "S/. 4|"
                "S/. 5|"
                "S/. 6|"
                "S/. 7|"
                "S/. 8|"
                "S/. 9|"
                "S/. 10|"
                "Vuelto $|"
                "$ 1|"
                "$ 2|"
                "$ 3|"
                "$ 4|"
                "$ 5|"
                "$ 6|"
                "$ 7|"
                "$ 8|"
                "$ 9|"
                "$ 10|"
                SKIP.

        FOR EACH ccbccaja WHERE
            ccbccaja.codcia = 1 AND
            CcbCCaja.CodDiv >= "" AND
            ccbccaja.coddoc >= "" AND
            CcbCCaja.FchDoc < TODAY AND
            year(fchdoc) = 2008 and
            ccbccaja.flgcie = "P" AND
            ccbccaja.usuario >= ""
            USE-INDEX LLAVE07 NO-LOCK:
            if ccbccaja.coddoc = "I/C" THEN DO:
                amtnac = impnac[1].
                amtusa = impusa[1].
            END.
            ELSE DO:
                amtnac = impnac[1] * -1.
                amtusa = impusa[1] * -1.
            END.
            PUT UNFORMATTED 
                CcbCCaja.CodDiv "|"
                CcbCCaja.FchDoc "|"
                ccbccaja.coddoc "|"
                ccbccaja.nrodoc "|"
                ccbccaja.tipo "|"
                ccbccaja.usuario "|"
                ccbccaja.codcli "|"
                ccbccaja.nomcli "|"
                flgest "|"
                ccbccaja.flgcie "|"
                fchcie "|"
                vuenac "|"
                amtnac "|"
                impnac[2] "|"
                impnac[3] "|"
                impnac[4] "|"
                impnac[5] "|"
                impnac[6] "|"
                impnac[7] "|"
                impnac[8] "|"
                impnac[9] "|"
                impnac[10] "|"
                vueusa "|"
                amtusa "|"
                impusa[2] "|"
                impusa[3] "|"
                impusa[4] "|"
                impusa[5] "|"
                impusa[6] "|"
                impusa[7] "|"
                impusa[8] "|"
                impusa[9] "|"
                impusa[10] "|"
                SKIP.
        end.
        output close.
