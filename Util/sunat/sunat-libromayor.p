    
    DEFINE VARIABLE pinta-mes  AS CHARACTER FORMAT "X(40)" NO-UNDO.
    DEFINE VARIABLE x-moneda   AS CHARACTER FORMAT "X(40)" NO-UNDO.
    DEFINE VARIABLE x-expres   AS CHARACTER FORMAT "X(40)" NO-UNDO.
    DEFINE VARIABLE x-fchast   AS CHARACTER FORMAT "X(5)" NO-UNDO.
    DEFINE VARIABLE x-fchdoc   AS CHARACTER FORMAT "X(5)" NO-UNDO.
    DEFINE VARIABLE x-fchvto   AS CHARACTER FORMAT "X(5)" NO-UNDO.
    DEFINE VARIABLE x-clfaux   LIKE cb-dmov.clfaux NO-UNDO.
    DEFINE VARIABLE x-codaux   LIKE cb-dmov.codaux NO-UNDO.
    DEFINE VARIABLE x-nroast   LIKE cb-dmov.nroast NO-UNDO.
    DEFINE VARIABLE x-codope   LIKE cb-dmov.codope NO-UNDO.
    DEFINE VARIABLE x-coddoc   LIKE cb-dmov.coddoc NO-UNDO.
    DEFINE VARIABLE x-nrodoc   LIKE cb-dmov.nrodoc NO-UNDO.
    DEFINE VARIABLE x-nroref   LIKE cb-dmov.nroref NO-UNDO.
    DEFINE VARIABLE x-glodoc   LIKE cb-dmov.glodoc NO-UNDO.
    DEFINE VARIABLE x-CodCta   LIKE cb-dmov.CodCta NO-UNDO.

    DEFINE VARIABLE x-Cco      LIKE cb-dmov.Cco    NO-UNDO.

    DEFINE VARIABLE x-debe     AS DECIMAL FORMAT "ZZZZ,ZZZ,ZZ9.99-" 
                               COLUMN-LABEL "M o v i m i!Cargos     " NO-UNDO. 
    DEFINE VARIABLE x-haber    AS DECIMAL FORMAT "ZZZZ,ZZZ,ZZ9.99-" 
                               COLUMN-LABEL "e n t o s      !Abonos     " NO-UNDO.
    DEFINE VARIABLE x-saldoi   AS DECIMAL FORMAT "ZZZZ,ZZZ,ZZ9.99-"
                               COLUMN-LABEL "Saldo     !Inicial    " NO-UNDO.
    DEFINE VARIABLE x-saldof   AS DECIMAL FORMAT "ZZZZ,ZZZ,ZZ9.99-"
                               COLUMN-LABEL "S a l d o!Deudor    " NO-UNDO.
    DEFINE VARIABLE x-deudor   AS DECIMAL FORMAT "ZZZZ,ZZZ,ZZ9.99-"
                               COLUMN-LABEL "S a l d o!Deudor    " NO-UNDO.
    DEFINE VARIABLE x-acreedor AS DECIMAL FORMAT "ZZZZ,ZZZ,ZZ9.99-" 
                               COLUMN-LABEL "A c t u a l    !Acreedor   " NO-UNDO.
    DEFINE VARIABLE x-importe AS DECIMAL FORMAT "->>>>>>>,>>9.99" 
                               COLUMN-LABEL "Importe S/." NO-UNDO. 
    DEFINE VARIABLE c-debe     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE c-haber    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE t-debe     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE t-haber    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE t-saldoi   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE t-saldof   AS DECIMAL NO-UNDO.

    DEFINE VARIABLE x-conreg   AS INTEGER NO-UNDO.

    DEFINE VARIABLE v-Saldoi   AS DECIMAL   EXTENT 10 NO-UNDO.
    DEFINE VARIABLE v-Debe     AS DECIMAL   EXTENT 10 NO-UNDO.
    DEFINE VARIABLE v-Haber    AS DECIMAL   EXTENT 10 NO-UNDO.
    DEFINE VARIABLE v-CodCta   AS CHARACTER EXTENT 10 NO-UNDO.
    
    DEF VAR con-ctas AS INTEGER.
    DEF VAR xi AS INTEGER INIT 0.
    DEF VAR No-tiene-mov AS LOGICAL. 
    DEF VAR y-codope AS CHAR.
    DEF VAR y-coddiv AS CHAR.
    DEF VAR x-auxiliar AS CHAR.
    DEF VAR G-NOMCTA AS CHAR FORMAT "X(60)".
    DEF VAR x-coddiv AS CHAR NO-UNDO.

    DEFINE        VARIABLE Max-Digitos AS INTEGER INIT 6 NO-UNDO.

    DEFINE BUFFER B-Cuentas FOR cb-ctas.

    ASSIGN y-codope = ''
           y-coddiv = ''
           x-auxiliar = ''
           x-coddiv   = ''.
    OUTPUT TO D:/Libro_mayor.txt.        
    FOR EACH cb-ctas NO-LOCK WHERE cb-ctas.codcia = 0
        /*
        AND cb-ctas.codcta >= x-cta-ini
        AND cb-ctas.codcta <= x-cta-fin
        -*/
        AND cb-ctas.CodCta <> ""
        /*AND cb-ctas.ClfAux BEGINS ""*/
        AND LENGTH( cb-ctas.CodCta ) = Max-Digitos
        BREAK BY (cb-ctas.Codcta):

        x-CodCta = cb-ctas.CodCta.
        ASSIGN c-debe   = 0
               c-haber  = 0
               x-saldoi = 0 
               xi       = 0 
               No-tiene-mov = yes.

        IF y-codope   = "" AND y-coddiv   = "" AND x-auxiliar = ""  THEN /* SALDO INICIAL */
            FOR EACH cb-dmov NO-LOCK WHERE cb-dmov.codcia  = 1
                AND cb-dmov.periodo = 2009 
                /*AND cb-dmov.nromes  < c-mes */
                AND cb-dmov.codcta  = x-CodCta :
                CASE 1 :
                    when 1 then if (not cb-dmov.tpomov) 
                            then x-saldoi = x-saldoi + cb-dmov.impmn1.
                    else x-saldoi = x-saldoi - cb-dmov.impmn1.     
                    when 2 then if (not cb-dmov.tpomov) 
                        then x-saldoi = x-saldoi + cb-dmov.impmn2.
                    else x-saldoi = x-saldoi - cb-dmov.impmn2.     
                END CASE.
            END.

        t-saldoi = t-saldoi + x-saldoi.
        ASSIGN 
            x-nroast = x-codcta
            x-GloDoc = ""
            x-fchast = ""
            x-codope = ""
            x-fchDoc = ""
            x-nroDoc = ""
            x-fchVto = ""
            x-nroref = "".

        x-GloDoc = "S a l d o  I n i c i a l " .
/*         IF x-saldoi <> 0 THEN DO:                             */
/*             xi = xi + 1.                                      */
/*             G-NOMCTA = cb-ctas.codcta + " " + cb-ctas.nomcta. */
/*             cColumn = STRING(iCount).                         */
/*             cRange = "A" + cColumn.                           */
/*             chWorkSheet:Range(cRange):Value = G-NOMCTA.       */
/*             iCount = iCount + 1.                              */
/*             cColumn = STRING(iCount).                         */
/*             cRange = "C" + cColumn.                           */
/*             chWorkSheet:Range(cRange):Value = x-fchast.       */
/*             cRange = "D" + cColumn.                           */
/*             chWorkSheet:Range(cRange):Value = x-nroast.       */
/*             cRange = "E" + cColumn.                           */
/*             chWorkSheet:Range(cRange):Value = x-codope.       */
/*             cRange = "H" + cColumn.                           */
/*             chWorkSheet:Range(cRange):Value = x-fchdoc.       */
/*             cRange = "J" + cColumn.                           */
/*             chWorkSheet:Range(cRange):Value = x-nrodoc.       */
/*             cRange = "K" + cColumn.                           */
/*             chWorkSheet:Range(cRange):Value = "".             */
/*             cRange = "L" + cColumn.                           */
/*             chWorkSheet:Range(cRange):Value = x-glodoc.       */
/*             cRange = "M" + cColumn.                           */
/*             chWorkSheet:Range(cRange):Value = x-saldoi.       */
/*             iCount = iCount + 1.                              */
/*                                                               */
/*                                                               */
/*                                                               */
/*         END.                                                  */
        FOR EACH cb-dmov NO-LOCK WHERE cb-dmov.codcia  = 1
            AND cb-dmov.periodo = 2009 
            /*AND cb-dmov.nromes  = c-mes*/
            /*AND cb-dmov.codope BEGINS ("")*/
            AND cb-dmov.codcta  = x-CodCta
            /***
            AND cb-dmov.clfaux  begins x-Clasificacion
            AND cb-dmov.codaux  begins x-auxiliar
            AND cb-dmov.coddiv  begins y-coddiv
            AND cb-dmov.cco     BEGINS FILL-IN-Cco
            ***/
            BREAK BY cb-dmov.CodOpe BY cb-dmov.NroAst BY cb-dmov.NroItm:

            No-tiene-mov = no.

            IF xi = 0 THEN DO:
               xi = xi + 1.
               G-NOMCTA = cb-ctas.codcta + " " + cb-ctas.nomcta.
/*                cColumn = STRING(iCount).                   */
/*                cRange = "A" + cColumn.                     */
/*                chWorkSheet:Range(cRange):Value = G-NOMCTA. */
/*                iCount = iCount + 1.                        */
            END.    

            /* Buscando la cabecera correspondiente */
            x-fchast = ?.
            FIND cb-cmov WHERE cb-cmov.codcia  = cb-dmov.codcia
                           AND cb-cmov.periodo = cb-dmov.periodo 
                           AND cb-cmov.nromes  = cb-dmov.nromes
                           AND cb-cmov.codope  = cb-dmov.codope
                           AND cb-cmov.nroast  = cb-dmov.nroast
                           NO-LOCK NO-ERROR.
             IF AVAILABLE cb-cmov THEN x-fchast = STRING(cb-cmov.fchast).
             ASSIGN x-glodoc = cb-dmov.glodoc
                    x-NroAst = cb-dmov.NroAst
                    x-CodOpe = cb-dmov.CodOpe
                    x-codaux = cb-dmov.codaux
                    x-NroDoc = cb-dmov.NroDoc
                    x-NroRef = cb-dmov.NroRef
                    x-coddiv = cb-dmov.CodDiv
                    x-clfaux = cb-dmov.clfaux
                    x-coddoc = cb-dmov.coddoc
                    x-cco    = cb-dmov.cco.

             IF x-glodoc = "" THEN IF AVAILABLE cb-cmov THEN x-glodoc = cb-cmov.notast.
             IF  x-glodoc = "" THEN DO:
                CASE cb-dmov.clfaux:
                    WHEN "@CL" THEN DO:
                        FIND gn-clie WHERE gn-clie.codcli = cb-dmov.codaux
                                        AND gn-clie.CodCia = 0
                                    NO-LOCK NO-ERROR. 
                        IF AVAILABLE gn-clie THEN
                            x-glodoc = gn-clie.nomcli.
                    END.
                    WHEN "@PV" THEN DO:
                        FIND gn-prov WHERE gn-prov.codpro = cb-dmov.codaux
                                        AND gn-prov.CodCia = 0
                                    NO-LOCK NO-ERROR.                      
                        IF AVAILABLE gn-prov THEN 
                            x-glodoc = gn-prov.nompro.
                    END.
                    WHEN "@CT" THEN DO:
                        find b-cuentas WHERE b-cuentas.codcta = cb-dmov.codaux
                                        AND b-cuentas.CodCia = 0
                                    NO-LOCK NO-ERROR.                      
                        IF AVAILABLE b-cuentas THEN x-glodoc = b-cuentas.nomcta.
                    END.
                    OTHERWISE DO:
                        FIND cb-auxi WHERE cb-auxi.clfaux = cb-dmov.clfaux
                                        AND cb-auxi.codaux = cb-dmov.codaux
                                        AND cb-auxi.CodCia = 0
                                    NO-LOCK NO-ERROR.                      
                        IF AVAILABLE cb-auxi THEN 
                            x-glodoc = cb-auxi.nomaux.
                    END.
                END CASE.
            END.
            IF NOT tpomov THEN DO:
                x-importe = ImpMn1.
                CASE 1:
                    WHEN 1 THEN DO:
                        x-debe  = ImpMn1.
                        x-haber = 0.
                    END.
                    WHEN 2 THEN DO:
                        x-debe  = ImpMn2.
                        x-haber = 0.
                    END.
                END CASE.
            END.
            ELSE DO:
                x-importe = ImpMn1 * -1. 
                CASE 1:
                    WHEN 1 THEN DO:
                        x-debe  = 0.
                        x-haber = ImpMn1.
                    END.
                    WHEN 2 THEN DO:
                        x-debe  = 0.
                        x-haber = ImpMn2.
                    END.
                END CASE.            
            END.
            IF NOT (x-haber = 0 AND x-debe = 0) AND x-debe <> ? AND x-haber <> ? THEN DO:
                x-fchdoc = STRING(cb-dmov.fchdoc).  
                x-fchvto = STRING(cb-dmov.fchvto).
                t-Debe  = t-Debe  + x-Debe.
                t-Haber = t-Haber + x-Haber.
                c-Debe  = c-Debe  + x-Debe.
                c-Haber = c-Haber + x-Haber.

                PUT UNFORMATTED
                    G-NOMCTA   "|"
                    x-coddiv   "|"
                    x-cco      "|"
                    cb-cmov.fchast "|"
                    x-nroast       "|"
                    x-codope      "|"
                    x-clfaux     "|"
                    x-codaux     "|"
                    cb-dmov.fchdoc  "|"
                    x-coddoc        "|"
                    x-nrodoc        "|"
                    cb-dmov.OrdCmp  "|"
                    x-glodoc        "|"
                    x-debe          "|"
                    x-haber         "|"
                    x-importe SKIP.
            END.            
        END.

/*         IF no-tiene-mov THEN NEXT.                    */
/*                                                       */
/*         ASSIGN x-Debe   = c-Debe                      */
/*                x-Haber  = c-Haber                     */
/*                x-SaldoF = x-SaldoI + x-Debe - x-Haber */
/*                x-nroast = x-codcta                    */
/*                x-fchast = ""                          */
/*                x-codope = ""                          */
/*                x-fchDoc = ""                          */
/*                x-nroDoc = "TOTAL"                     */
/*                x-nroref = x-CodCta                    */
/*                x-GloDoc = cb-ctas.NomCta.             */
/*         iCount = iCount + 1.                          */
/*         cColumn = STRING(iCount).                     */
/*         cRange = "K" + cColumn.                       */
/*         chWorkSheet:Range(cRange):Value = "".         */
/*         cRange = "L" + cColumn.                       */
/*         chWorkSheet:Range(cRange):Value = x-glodoc.   */
/*         cRange = "M" + cColumn.                       */
/*         chWorkSheet:Range(cRange):Value = x-debe.     */
/*         cRange = "N" + cColumn.                       */
/*         chWorkSheet:Range(cRange):Value = x-haber.    */
/*         cRange = "P" + cColumn.                       */
/*         chWorkSheet:Range(cRange):Value = x-saldof.   */
/*         iCount = iCount + 1.                          */
/*         con-ctas = con-ctas + 1 .                     */
    END.

OUTPUT TO CLOSE.
