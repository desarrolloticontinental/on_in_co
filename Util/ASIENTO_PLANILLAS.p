
DEFINE VARIABLE p-codcia  AS INTEGER INIT 1.
DEFINE VARIABLE p-periodo AS INTEGER INIT 2007.
DEFINE VARIABLE p-mes     AS INTEGER INIT 12.
DEFINE VARIABLE p-codope  AS CHARACTER INIT "063".
DEFINE VARIABLE p-nroast  AS CHARACTER.
DEFINE VARIABLE p-codpln  AS INTEGER INIT 1.
DEFINE VARIABLE p-codcal  AS INTEGER INIT 1.

DEFINE VARIABLE t-tpomov AS LOGICAL.
DEFINE VARIABLE a-tpomov AS LOGICAL.
DEFINE VARIABLE x-ctrcta AS CHARACTER.
DEFINE VARIABLE m-clfaux AS CHARACTER.
DEFINE VARIABLE n-clfaux AS CHARACTER.
DEFINE VARIABLE x-cta    AS CHARACTER.
DEFINE VARIABLE x-imptot AS INTEGER.
DEFINE VARIABLE x-coddiv AS CHAR.
DEFINE VARIABLE x-cco    AS CHAR.

DEFINE TEMP-TABLE t-prev
    FIELD codcta AS CHARACTER
    FIELD ctacta AS CHARACTER
    FIELD coddiv AS CHARACTER
    FIELD cco    AS CHARACTER
    FIELD nomcta AS CHARACTER
    FIELD tpomov AS LOGICAL
    FIELD clfaux AS CHAR
    FIELD codaux AS CHAR
    FIELD impmn1 AS DECIMAL
    FIELD impmn2 AS DECIMAL
    INDEX IDX01 coddiv codcta
    INDEX IDX02 codcta codaux.

x-ctrcta = "411101".
x-imptot = 403.
x-coddiv = "00000".

CASE p-codcal:
    WHEN 009 THEN x-ctrcta = '471101'.      /* CTS MENSUAL */
    WHEN 010 THEN x-ctrcta = '412102'.      /* PROV VAC TRUNCAS */
    WHEN 011 THEN x-ctrcta = '411302'.      /* PROV GRATIF TRUNCAS */
    OTHERWISE x-ctrcta = '411101'.
END CASE.    

FOR EACH t-prev:
    DELETE t-prev.
END.

FOR EACH PL-BOLE NO-LOCK WHERE
    PL-BOLE.codpln = p-codpln AND
    PL-BOLE.codcal = p-codcal AND
    PL-BOLE.codcta <> "" BREAK BY PL-BOLE.tpobol DESCENDING.
    IF FIRST-OF ( PL-BOLE.tpobol ) THEN DO:
        CASE PL-BOLE.tpobol:
            WHEN "Aportes" OR
            WHEN "Descuentos"     THEN t-tpomov = TRUE.
            WHEN "Remuneraciones" THEN t-tpomov = FALSE.
        END CASE.
        a-tpomov = NOT t-tpomov.
    END.
    MOVIMIENTOS:
    FOR EACH PL-MOV-MES WHERE
        PL-MOV-MES.codcia  = p-codcia AND
        PL-MOV-MES.periodo = p-periodo AND
        PL-MOV-MES.nromes  = p-mes AND
        PL-MOV-MES.codpln  = p-codpln AND
        PL-MOV-MES.codcal  = p-codcal AND
        PL-MOV-MES.codmov  = PL-BOLE.codmov NO-LOCK:
        FIND PL-FLG-MES WHERE
            PL-FLG-MES.codcia  = p-codcia AND
            PL-FLG-MES.periodo = p-periodo AND
            PL-FLG-MES.nromes  = p-mes AND
            PL-FLG-MES.codpln  = PL-MOV-MES.codpln AND
            PL-FLG-MES.codper  = PL-MOV-MES.codper NO-LOCK NO-ERROR.
        IF AVAILABLE PL-FLG-MES /* AND PL-FLG-MES.sitact <> "Inactivo" */ THEN DO:
            n-clfaux = ''.
            m-clfaux = ''.
            x-cco    = PL-FLG-MES.Ccosto.
            /******************07-06-2002 MGM*************/            
            x-cta    = PL-BOLE.CodCta.            
            IF PL-BOLE.Tpobol = "Remuneraciones" THEN DO:
               x-cta = SUBSTR(PL-FLG-MES.Clase,1,2) + SUBSTR(PL-BOLE.Codcta,3,4).
            END.
            FIND cb-ctas WHERE
                 cb-ctas.codcia = 0 AND
                 cb-ctas.codcta = x-cta /*PL-BOLE.codcta*/ NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Cb-Ctas THEN DO:
                MESSAGE 'Cuenta no configurada:' x-cta SKIP
                        'Para el personal:' PL-FLG-MES.CodPer
                        VIEW-AS ALERT-BOX ERROR.
/*MLR* 30/01/08 ***
                NEXT MOVIMIENTOS.
*MLR* ***/
            END.
            /*******************************************/
            IF AVAILABLE cb-ctas AND cb-ctas.PidAux THEN DO:
               ASSIGN
                   n-clfaux = "00" + PL-FLG-MES.codper
                   m-clfaux = 'PE'.
               IF x-cta BEGINS '46' THEN DO:
                  FIND PL-AFPS WHERE PL-AFPS.codafp = PL-FLG-MES.codafp NO-LOCK NO-ERROR.
                  IF AVAILABLE PL-AFPS THEN
                     ASSIGN
                         n-clfaux =  STRING(PL-AFPS.codafp,"99")
                         m-clfaux = '@AFP'.
               END.
            END.
            IF AVAILABLE cb-ctas AND cb-ctas.pidcco = NO THEN x-cco = ''.
            FIND t-prev WHERE
                t-prev.coddiv = x-coddiv AND
                t-prev.codcta = x-cta /*PL-BOLE.codcta*/ AND
                t-prev.tpomov = t-tpomov AND
                t-prev.codaux = n-clfaux AND
                t-prev.clfaux = m-clfaux AND 
                t-prev.cco    = x-cco NO-ERROR.
            IF NOT AVAILABLE t-prev THEN DO:
                CREATE t-prev.
                t-prev.coddiv = x-coddiv.                 
                t-prev.codcta = x-cta /*PL-BOLE.codcta*/.
                t-prev.tpomov = t-tpomov.
                t-prev.clfaux = m-clfaux.
                t-prev.codaux = n-clfaux.
                t-prev.cco    = x-cco.
                FIND cb-ctas WHERE
                    cb-ctas.codcia = 0 AND
                    cb-ctas.codcta = t-prev.codcta NO-LOCK NO-ERROR.
                IF AVAILABLE cb-ctas THEN DO:
                   t-prev.nomcta = cb-ctas.nomcta.
                END.
                ELSE t-prev.nomcta = "CUENTA NO EXISTE !!!".
            END.
            t-prev.impmn1 = t-prev.impmn1 + PL-MOV-MES.valcal-mes.
            IF PL-BOLE.codcta-cntr = "" THEN DO:
               NEXT.
            END.
            ELSE DO:
                n-clfaux = ''.
                m-clfaux = ''.
                x-cta    = PL-BOLE.CodCta-cntr.            
                x-cco    = PL-FLG-MES.Ccosto.
                IF PL-BOLE.Tpobol = "Aportes" THEN DO:
                   x-cta = SUBSTR(PL-FLG-MES.Clase,1,2) + SUBSTR(PL-BOLE.Codcta-cntr,3,4).
                END.

                FIND cb-ctas WHERE
                     cb-ctas.codcia = 0 AND
                     cb-ctas.codcta = x-cta /*PL-BOLE.codcta-cntr*/ NO-LOCK NO-ERROR.
                IF AVAILABLE cb-ctas AND cb-ctas.PidAux THEN DO:
                   ASSIGN
                       n-clfaux = "00" + PL-FLG-MES.codper
                       m-clfaux = 'PE'.
                   IF x-cta /*PL-BOLE.codcta-cntr*/ BEGINS '46' THEN DO:
                      FIND PL-AFPS WHERE PL-AFPS.codafp = PL-FLG-MES.codafp NO-LOCK NO-ERROR.
                      IF AVAILABLE PL-AFPS THEN
                         ASSIGN
                             n-clfaux = STRING(PL-AFPS.codafp,"99")
                             m-clfaux = '@AFP'.
                   END.
                END.
                IF AVAILABLE cb-ctas AND cb-ctas.pidcco = NO THEN x-cco = ''.
                FIND t-prev WHERE
                    t-prev.coddiv = x-coddiv AND                
                    t-prev.codcta = x-cta /*PL-BOLE.codcta-cntr*/ AND
                    t-prev.tpomov = a-tpomov AND
                    t-prev.codaux = n-clfaux AND
                    t-prev.clfaux = m-clfaux AND
                    t-prev.cco    = x-cco NO-ERROR.                    
                IF NOT AVAIL t-prev THEN DO:
                    CREATE t-prev.
                    t-prev.coddiv = x-coddiv.                    
                    t-prev.codcta = x-cta /*PL-BOLE.codcta-CNTR*/.
                    t-prev.tpomov = a-tpomov.
                    t-prev.clfaux = m-clfaux.
                    t-prev.codaux = n-clfaux.
                    t-prev.cco    = x-cco.
                    FIND cb-ctas WHERE
                        cb-ctas.codcia = 0 AND
                        cb-ctas.codcta = t-prev.codcta NO-LOCK NO-ERROR.
                    IF AVAILABLE cb-ctas THEN DO:
                       t-prev.nomcta = cb-ctas.nomcta.
                    END.
                    ELSE t-prev.nomcta = "CUENTA NO EXISTE !!!".
                END.
            END.
            t-prev.impmn1 = t-prev.impmn1 + PL-MOV-MES.valcal-mes.
        END.
    END.
END.


n-clfaux = ''.
m-clfaux = ''.
a-tpomov = TRUE.

FOR EACH PL-MOV-MES WHERE
    PL-MOV-MES.codcia  = p-codcia AND
    PL-MOV-MES.periodo = p-periodo AND
    PL-MOV-MES.nromes  = p-mes AND
    PL-MOV-MES.codpln  = p-codpln AND
    PL-MOV-MES.codcal  = p-codcal AND
    PL-MOV-MES.codmov  = x-imptot NO-LOCK:
    FIND PL-FLG-MES WHERE
         PL-FLG-MES.codcia  = p-codcia AND
         PL-FLG-MES.periodo = p-periodo AND
         PL-FLG-MES.nromes  = p-mes AND
         PL-FLG-MES.codpln  = PL-MOV-MES.codpln AND
         PL-FLG-MES.codper  = PL-MOV-MES.codper NO-LOCK NO-ERROR.
    IF AVAILABLE PL-FLG-MES THEN DO:
        FIND t-prev WHERE
            t-prev.coddiv = x-coddiv AND    
            t-prev.codcta = x-ctrcta AND
            t-prev.tpomov = a-tpomov AND
            t-prev.codaux = n-clfaux AND
            t-prev.clfaux = m-clfaux NO-ERROR.                    
        IF NOT AVAIL t-prev THEN DO:
            CREATE t-prev.
            t-prev.coddiv = x-coddiv.            
            t-prev.codcta = x-ctrcta.
            t-prev.tpomov = a-tpomov.
            t-prev.clfaux = m-clfaux.
            t-prev.codaux = n-clfaux.
            FIND cb-ctas WHERE
                cb-ctas.codcia = 0 AND
                cb-ctas.codcta = t-prev.codcta NO-LOCK NO-ERROR.
            IF AVAILABLE cb-ctas THEN DO:
               t-prev.nomcta = cb-ctas.nomcta.
            END.
            ELSE t-prev.nomcta = "CUENTA NO EXISTE !!!".
        END.
        t-prev.impmn1 = t-prev.impmn1 + PL-MOV-MES.valcal-mes.
    END.
END.


