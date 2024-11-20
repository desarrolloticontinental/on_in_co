/* CREACION DE CODIGOS AUTOMATICOS EL ALMMMATG */

DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR iCuenta AS INT NO-UNDO.
DEF VAR x-NroCor AS INT NO-UNDO.
DEF VAR x-OrdMat AS INT NO-UNDO.
DEF VAR c-Alm AS CHAR NO-UNDO.

DEF BUFFER MATG FOR Almmmatg.

/* Capturamos Correlativo */
DO iCuenta = 1 TO 400:
    FIND LAST MATG WHERE MATG.CodCia = S-CODCIA NO-LOCK NO-ERROR.
    IF AVAILABLE MATG THEN x-NroCor = INTEGER(MATG.codmat) + 1.
    ELSE x-NroCor = 1.

    FIND LAST MATG WHERE MATG.Codcia = S-CODCIA 
        AND  MATG.CodFam = "888"
        USE-INDEX Matg08 NO-LOCK NO-ERROR.
    IF AVAILABLE MATG 
        THEN x-ordmat = MATG.Orden + 3.
    ELSE x-ordmat = 1.

    CREATE Almmmatg.
    ASSIGN
        Almmmatg.codcia = s-codcia
        Almmmatg.codmat = STRING(x-NroCor,"999999")
        Almmmatg.desmat = "POR DEFINIR " + Almmmatg.codmat
        Almmmatg.orden  = x-ordmat
        Almmmatg.ordlis = x-ordmat
        Almmmatg.tpoart = 'A'     /* Activo */
        Almmmatg.FchIng = TODAY
        Almmmatg.FchAct = TODAY
        Almmmatg.codfam = '888'
        Almmmatg.subfam = '888'
        Almmmatg.CodSSFam = '999'
        Almmmatg.aftigv = YES
        Almmmatg.aftisc = NO
        Almmmatg.catconta[1] = "MC"
        Almmmatg.UndA = "UNI"
        Almmmatg.UndBas = "UNI"
        Almmmatg.UndCmp = "UNI"
        Almmmatg.UndStk = "UNI"
        Almmmatg.CHR__01 = "UNI"
        Almmmatg.CHR__02 = "T"
        Almmmatg.UndAlt[1] = "UNI"
        Almmmatg.PreOfi = 0.01.
    /* Actualizamos la lista de Almacenes */ 
/*     C-ALM = ''.                                                                                    */
/*     FOR EACH Almacen NO-LOCK WHERE Almacen.CodCia = Almmmatg.codcia AND Almacen.TdoArt:            */
/*         /* CONSISTENCIA POR PRODUCTO Y ALMACEN */                                                  */
/*         IF Almmmatg.TpoMrg <> '' AND Almacen.Campo-c[2] <> '' THEN DO:                             */
/*             IF Almmmatg.TpoMrg <> Almacen.Campo-c[2] THEN NEXT.                                    */
/*         END.                                                                                       */
/*         /* *********************************** */                                                  */
/*         IF C-ALM = "" THEN C-ALM = TRIM(Almacen.CodAlm).                                           */
/*         IF LOOKUP(TRIM(Almacen.CodAlm),C-ALM) = 0 THEN C-ALM = C-ALM + "," + TRIM(Almacen.CodAlm). */
/*     END.                                                                                           */
/*     ASSIGN                                                                                         */
/*         Almmmatg.almacenes = C-ALM.                                                                */
    /* RHC 24-09-2013 ACTUALIZAMOS MONEDA DE VENTA Y TIPO DE CAMBIO */
    FIND Almtfami OF Almmmatg NO-LOCK NO-ERROR.
    IF AVAILABLE Almtfami THEN DO:
       ASSIGN
           Almmmatg.tpocmb = Almtfami.tpocmb.
    END.
    IF Almmmatg.MonVta = 0 THEN Almmmatg.MonVta = 1.

/*     RUN ACTUALIZA-MAT-x-ALM NO-ERROR.              */
/*     IF ERROR-STATUS:ERROR THEN UNDO, RETURN ERROR. */

END.


PROCEDURE ACTUALIZA-MAT-x-ALM:

    FOR EACH Almacen NO-LOCK WHERE Almacen.CodCia = Almmmatg.codcia AND Almacen.TdoArt
        TRANSACTION ON ERROR UNDO, RETURN ERROR ON STOP UNDO, RETURN ERROR:

        /* CONSISTENCIA POR PRODUCTO Y ALMACEN */
        IF Almmmatg.TpoMrg <> '' AND Almacen.Campo-c[2] <> '' THEN DO:
            IF Almmmatg.TpoMrg <> Almacen.Campo-c[2] THEN NEXT.
        END.
        /* *********************************** */
        FIND Almmmate WHERE Almmmate.CodCia = Almmmatg.codcia AND 
             Almmmate.CodAlm = Almacen.CodAlm AND 
             Almmmate.CodMat = Almmmatg.CodMat NO-ERROR.
        IF NOT AVAILABLE Almmmate THEN DO:
           CREATE Almmmate.
           ASSIGN Almmmate.CodCia = Almmmatg.codcia
                  Almmmate.CodAlm = Almacen.CodAlm
                  Almmmate.CodMat = Almmmatg.CodMat.
        END.
        ASSIGN Almmmate.DesMat = Almmmatg.DesMat
               Almmmate.FacEqu = Almmmatg.FacEqu
               Almmmate.UndVta = Almmmatg.UndStk
               Almmmate.CodMar = Almmmatg.CodMar.
        FIND FIRST almautmv WHERE 
             almautmv.CodCia = Almmmatg.codcia AND
             almautmv.CodFam = Almmmatg.codfam AND
             almautmv.CodMar = Almmmatg.codMar AND
             almautmv.Almsol = Almmmate.CodAlm NO-LOCK NO-ERROR.
        IF AVAILABLE almautmv THEN 
           ASSIGN Almmmate.AlmDes = almautmv.Almdes
                  Almmmate.CodUbi = almautmv.CodUbi.
    END.


END PROCEDURE.
