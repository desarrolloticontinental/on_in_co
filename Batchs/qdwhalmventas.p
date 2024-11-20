&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Variables iniciales */

DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR pv-codcia AS INT INIT 000 NO-UNDO.

DEF VAR x-FchIni AS DATE NO-UNDO.
DEF VAR x-FchFin AS DATE NO-UNDO.

/* Primer día del mes */
x-FchIni = TODAY - DAY(TODAY) + 1.
/* Ultimo día del mes */
x-FchFin = x-FchIni - 1.
/* 1 mes atrás */
x-FchIni = ADD-INTERVAL(x-FchIni, -1, 'months').

/*-x-FchIni = 01/01/2018.*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 6
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

PUT UNFORMATTED 'Borra estadisticas ' NOW SKIP.
RUN Borra-Estadisticas.

PUT UNFORMATTED 'Carga estadisticas ' NOW SKIP.
RUN Carga-Estadisticas.

/* PUT UNFORMATTED 'Datos finales ' NOW SKIP. */
/* RUN Datos-Finales.                         */

PUT UNFORMATTED 'Fin de proceso ' NOW SKIP.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Borra-Estadisticas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Borra-Estadisticas Procedure 
PROCEDURE Borra-Estadisticas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR x-PeriodoIni AS INT NO-UNDO.
DEF VAR x-PeriodoFin AS INT NO-UNDO.
ASSIGN
    x-PeriodoIni = YEAR(x-FchIni)
    x-PeriodoFin = YEAR(x-FchFin).
DEF VAR x-Fecha AS DATE NO-UNDO.

DO x-Fecha = x-FchIni TO x-FchFin:
    IF DAY(x-Fecha) = 01 THEN DO:   /* Primer dia del mes */
        FOR EACH almacen_ventas WHERE almacen_ventas.CodCia = s-CodCia
            AND almacen_ventas.Periodo = YEAR(x-Fecha) EXCLUSIVE-LOCK:
            almacen_ventas.Cantidad[MONTH(x-Fecha)] = 0.
        END.
    END.
END.
IF AVAILABLE(almacen_ventas) THEN RELEASE almacen_ventas.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Carga-Estadisticas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Estadisticas Procedure 
PROCEDURE Carga-Estadisticas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* Primero las ventas */
DEF VAR x-Mes AS INT NO-UNDO.

FOR EACH Almacen NO-LOCK WHERE Almacen.codcia = s-CodCia,
    EACH Almcmov NO-LOCK WHERE Almcmov.CodCia = s-CodCia
    AND Almcmov.CodAlm = Almacen.CodAlm
    AND Almcmov.TipMov = "S"
    AND Almcmov.CodMov = 02     /* Ventas */
    AND Almcmov.FchDoc >= x-FchIni AND Almcmov.FchDoc <= x-FchFin,
    EACH Almdmov OF Almcmov NO-LOCK,
    FIRST Almmmatg OF Almdmov NO-LOCK,
    FIRST Almtfami OF Almmmatg NO-LOCK,
    FIRST Almsfami OF Almmmatg NO-LOCK:
    FIND FIRST almacen_ventas WHERE almacen_ventas.CodCia = Almdmov.codcia
        AND almacen_ventas.CodMat = Almdmov.codmat
        AND almacen_ventas.CodAlm = Almdmov.codalm
        AND almacen_ventas.Periodo = YEAR(Almdmov.fchdoc)
        EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF NOT AVAILABLE almacen_ventas AND LOCKED(almacen_ventas) THEN DO:
        PUT UNFORMATTED 'ERROR VENTAS almacen: ' almdmov.codalm ' articulo: ' almdmov.codmat ' periodo: ' YEAR(Almdmov.fchdoc) SKIP.
        NEXT.
    END.
    IF NOT AVAILABLE almacen_ventas THEN DO:
        CREATE almacen_ventas.
        ASSIGN
            almacen_ventas.CodCia = Almdmov.codcia
            almacen_ventas.CodAlm = Almdmov.codalm
            almacen_ventas.CodMat = Almdmov.codmat
            almacen_ventas.Periodo = YEAR(Almdmov.fchdoc).
    END.
    x-Mes = MONTH(Almdmov.fchdoc).
    ASSIGN 
        almacen_ventas.Cantidad[x-Mes] = almacen_ventas.Cantidad[x-Mes] + (Almdmov.candes * Almdmov.factor).
    ASSIGN
        almacen_ventas.CodFam = Almmmatg.codfam
        almacen_ventas.DesMar = Almmmatg.desmar
        almacen_ventas.DesMat = Almmmatg.desmat
        almacen_ventas.SubFam = Almmmatg.subfam
        almacen_ventas.TpoArt = Almmmatg.tpoart
        almacen_ventas.UndBas = Almmmatg.undbas.
    FIND gn-prov WHERE gn-prov.codcia = pv-codcia
        AND gn-prov.codpro = Almmmatg.codpr1
        NO-LOCK NO-ERROR.
    IF AVAILABLE gn-prov THEN almacen_ventas.NomPro = gn-prov.nompro.
    ELSE almacen_ventas.NomPro = ''.
    FIND TabGener WHERE TabGener.CodCia = s-CodCia
        AND TabGener.Clave = 'ZG'
        AND TabGener.Libre_c01 = almacen_ventas.CodAlm
        AND CAN-FIND ( FIRST almtabla WHERE almtabla.Tabla = 'ZG'
                       AND almtabla.Codigo = TabGener.Codigo NO-LOCK)
        NO-LOCK NO-ERROR.
    IF AVAILABLE TabGener THEN almacen_ventas.Grupo = TabGener.Codigo.
    ELSE almacen_ventas.Grupo = ''.
END.
IF available(almacen_ventas) THEN RELEASE almacen_ventas.

/* Segundo las devoluciones */
FOR EACH Almacen NO-LOCK WHERE Almacen.codcia = s-CodCia,
    EACH Almcmov NO-LOCK WHERE Almcmov.CodCia = s-CodCia
    AND Almcmov.CodAlm = Almacen.CodAlm
    AND Almcmov.TipMov = "I"
    AND Almcmov.CodMov = 09     /* Devoluciones */
    AND Almcmov.FchDoc >= x-FchIni AND Almcmov.FchDoc <= x-FchFin,
    EACH Almdmov OF Almcmov NO-LOCK,
    FIRST Almmmatg OF Almdmov NO-LOCK,
    FIRST Almtfami OF Almmmatg NO-LOCK,
    FIRST Almsfami OF Almmmatg NO-LOCK:
    FIND FIRST almacen_ventas WHERE almacen_ventas.CodCia = Almdmov.codcia
        AND almacen_ventas.CodMat = Almdmov.codmat
        AND almacen_ventas.CodAlm = Almdmov.codalm
        AND almacen_ventas.Periodo = YEAR(Almdmov.fchdoc)
        EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF NOT AVAILABLE almacen_ventas AND LOCKED(almacen_ventas) THEN DO:
        PUT UNFORMATTED 'ERROR DEVOLUCIONES almacen: ' almdmov.codalm ' articulo: ' almdmov.codmat ' periodo: ' YEAR(Almdmov.fchdoc) SKIP.
        NEXT.
    END.
    IF NOT AVAILABLE almacen_ventas THEN DO:
        CREATE almacen_ventas.
        ASSIGN
            almacen_ventas.CodCia = Almdmov.codcia
            almacen_ventas.CodAlm = Almdmov.codalm
            almacen_ventas.CodMat = Almdmov.codmat
            almacen_ventas.Periodo = YEAR(Almdmov.fchdoc).
    END.
    x-Mes = MONTH(Almdmov.fchdoc).
    ASSIGN 
        almacen_ventas.Cantidad[x-Mes] = almacen_ventas.Cantidad[x-Mes] - (Almdmov.candes * Almdmov.factor).
    ASSIGN
        almacen_ventas.CodFam = Almmmatg.codfam
        almacen_ventas.DesMar = Almmmatg.desmar
        almacen_ventas.DesMat = Almmmatg.desmat
        almacen_ventas.SubFam = Almmmatg.subfam
        almacen_ventas.TpoArt = Almmmatg.tpoart
        almacen_ventas.UndBas = Almmmatg.undbas.
    FIND gn-prov WHERE gn-prov.codcia = pv-codcia
        AND gn-prov.codpro = Almmmatg.codpr1
        NO-LOCK NO-ERROR.
    IF AVAILABLE gn-prov THEN almacen_ventas.NomPro = gn-prov.nompro.
    ELSE almacen_ventas.NomPro = ''.
    FIND TabGener WHERE TabGener.CodCia = s-CodCia
        AND TabGener.Clave = 'ZG'
        AND TabGener.Libre_c01 = almacen_ventas.CodAlm
        AND CAN-FIND ( FIRST almtabla WHERE almtabla.Tabla = 'ZG'
                       AND almtabla.Codigo = TabGener.Codigo NO-LOCK)
        NO-LOCK NO-ERROR.
    IF AVAILABLE TabGener THEN almacen_ventas.Grupo = TabGener.Codigo.
    ELSE almacen_ventas.Grupo = ''.
END.
IF available(almacen_ventas) THEN RELEASE almacen_ventas.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Datos-Finales) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Datos-Finales Procedure 
PROCEDURE Datos-Finales :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH almacen_ventas EXCLUSIVE-LOCK WHERE almacen_ventas.CodCia = s-CodCia,
    FIRST Almmmatg OF almacen_ventas NO-LOCK,
    FIRST Almtfami OF Almmmatg NO-LOCK,
    FIRST Almsfami OF Almmmatg NO-LOCK:
    ASSIGN
        almacen_ventas.CodFam = Almmmatg.codfam
        almacen_ventas.DesMar = Almmmatg.desmar
        almacen_ventas.DesMat = Almmmatg.desmat
        almacen_ventas.SubFam = Almmmatg.subfam
        almacen_ventas.TpoArt = Almmmatg.tpoart
        almacen_ventas.UndBas = Almmmatg.undbas.
    FIND gn-prov WHERE gn-prov.codcia = pv-codcia
        AND gn-prov.codpro = Almmmatg.codpr1
        NO-LOCK NO-ERROR.
    IF AVAILABLE gn-prov THEN almacen_ventas.NomPro = gn-prov.nompro.
    ELSE almacen_ventas.NomPro = ''.
    FIND TabGener WHERE TabGener.CodCia = s-CodCia
        AND TabGener.Clave = 'ZG'
        AND TabGener.Libre_c01 = almacen_ventas.CodAlm
        AND CAN-FIND ( FIRST almtabla WHERE almtabla.Tabla = 'ZG'
                       AND almtabla.Codigo = TabGener.Codigo NO-LOCK)
        NO-LOCK NO-ERROR.
    IF AVAILABLE TabGener THEN almacen_ventas.Grupo = TabGener.Codigo.
    ELSE almacen_ventas.Grupo = ''.
END.
IF available(almacen_ventas) THEN RELEASE almacen_ventas.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

