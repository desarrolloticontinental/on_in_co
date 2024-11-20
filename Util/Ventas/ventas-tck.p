DEF VAR COMBO-BOX-Cupon AS CHAR.
DEF VAR COMBO-BOX-NomCupon AS CHAR.
DEF VAR FILL-IN-NewImport AS DEC.
DEF VAR s-codcia AS INT INIT 001.
DEF VAR x-impdto2 AS DEC.
DEF VAR y-ImpDto2 AS DEC.

DEFINE TEMP-TABLE ITEM NO-UNDO LIKE FacDPedi.
DEFINE TEMP-TABLE T-CDOCU NO-UNDO LIKE CcbCDocu.
DEFINE TEMP-TABLE T-DDOCU NO-UNDO LIKE CcbDDocu.
DEFINE BUFFER B-TABLA FOR VtaDTabla.
DEFINE TEMP-TABLE Promocion LIKE VtaDTabla.


OUTPUT TO d:\tmp\ventas-vales-utilex.txt.
PUT UNFORMATTED 'DIVISION|FECHA|COMPROBANTE|CLIENTE|NOMBRE|TIPO VALE|IMPORTE|RECALCULADO' SKIP.
FOR EACH vtadtickets NO-LOCK WHERE vtadtickets.codcia = 1 
    and date(vtadtickets.fecha) >= 01/01/2016
    AND vtadtickets.codpro = '10003814'
    AND vtadtickets.producto = '0005',
    FIRST vtatabla NO-LOCK WHERE vtatabla.codcia = 001
    AND vtatabla.tabla = 'VUTILEXTCK'
    AND vtatabla.llave_c3 = vtadtickets.producto
    AND vtatabla.llave_c5 = vtadtickets.nrotck
    BREAK BY vtadtickets.coddiv BY vtadtickets.codref BY vtadtickets.nroref:
    IF FIRST-OF(vtadtickets.coddiv)
        OR FIRST-OF(vtadtickets.codref)
        OR FIRST-OF(vtadtickets.nroref) THEN DO:
        FOR EACH ccbdcaja NO-LOCK WHERE ccbdcaja.codcia = vtadtickets.codcia
            AND ccbdcaja.coddiv = vtadtickets.coddiv
            AND ccbdcaja.coddoc = vtadtickets.codref
            AND ccbdcaja.nrodoc = vtadtickets.nroref,
            FIRST ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = 001
            AND ccbcdocu.coddoc = ccbdcaja.codref
            AND ccbcdocu.nrodoc = ccbdcaja.nroref:
            ASSIGN 
                COMBO-BOX-Cupon = '896314' 
                COMBO-BOX-NomCupon = 'Terceros'.
            IF LOOKUP(vtatabla.libre_c01, '20100038146,20511358907') > 0
                THEN ASSIGN 
                COMBO-BOX-Cupon = '321496' 
                COMBO-BOX-NomCupon = 'Colaboradores'.
            RUN Calculamos.
            PUT UNFORMATTED
                ccbcdocu.coddiv '|'
                ccbcdocu.fchdoc '|'
                ccbcdocu.coddoc ' ' ccbcdocu.nrodoc '|'
                ccbcdocu.codcli '|'
                ccbcdocu.nomcli '|'
                COMBO-BOX-NomCupon  '|'
                ccbcdocu.imptot '|'
                FILL-IN-NewImport
                SKIP.
        END.
    END.
END.

/* PARTE 2 */
FOR EACH vtadtickets NO-LOCK WHERE vtadtickets.codcia = 0
    and date(vtadtickets.fecha) >= 01/01/2016
    AND vtadtickets.codpro = ''
    AND vtadtickets.producto = '',
    FIRST vtatabla NO-LOCK WHERE vtatabla.codcia = 001
    AND vtatabla.tabla = 'VUTILEXTCK'
    AND vtatabla.llave_c3 = '0005'
    AND vtatabla.llave_c5 = vtadtickets.nrotck
    BREAK BY vtadtickets.coddiv BY vtadtickets.codref BY vtadtickets.nroref:
    IF FIRST-OF(vtadtickets.coddiv)
        OR FIRST-OF(vtadtickets.codref)
        OR FIRST-OF(vtadtickets.nroref) THEN DO:
        FOR EACH ccbdcaja NO-LOCK WHERE ccbdcaja.codcia = 001
            AND ccbdcaja.coddiv = vtadtickets.coddiv
            AND ccbdcaja.coddoc = vtadtickets.codref
            AND ccbdcaja.nrodoc = vtadtickets.nroref,
            FIRST ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = 001
            AND ccbcdocu.coddoc = ccbdcaja.codref
            AND ccbcdocu.nrodoc = ccbdcaja.nroref:
            ASSIGN 
                COMBO-BOX-Cupon = '896314' 
                COMBO-BOX-NomCupon = 'Terceros'.
            IF LOOKUP(vtatabla.libre_c01, '20100038146,20511358907') > 0
                THEN ASSIGN 
                COMBO-BOX-Cupon = '321496' 
                COMBO-BOX-NomCupon = 'Colaboradores'.
            RUN Calculamos.
            PUT UNFORMATTED
                ccbcdocu.coddiv '|'
                ccbcdocu.fchdoc '|'
                ccbcdocu.coddoc ' ' ccbcdocu.nrodoc '|'
                ccbcdocu.codcli '|'
                ccbcdocu.nomcli '|'
                COMBO-BOX-NomCupon  '|'
                ccbcdocu.imptot '|'
                FILL-IN-NewImport
                SKIP.
        END.
    END.
END.


PROCEDURE Calculamos:

    FIND VtaCTabla  WHERE VtaCTabla.CodCia = s-codcia
        AND VtaCTabla.Tabla = "UTILEX-ENCARTE"
        AND VtaCTabla.Llave = COMBO-BOX-Cupon
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE VtaCTabla THEN DO:
        MESSAGE 'Código del Cupón de Descuento NO válido' VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.

    DEF VAR pExcepcion AS LOG.
    DEF VAR pPorDto2 AS DEC NO-UNDO.
    DEF VAR pPreUni  AS DEC NO-UNDO.
    DEF VAR pLibre_c04 AS CHAR NO-UNDO.

    EMPTY TEMP-TABLE ITEM.

    FOR EACH Ccbddocu OF Ccbcdocu NO-LOCK:
        CREATE ITEM.
        BUFFER-COPY Ccbddocu TO ITEM
            ASSIGN
            ITEM.CanPed = Ccbddocu.candes.
    END.

    &SCOPED-DEFINE Rutina-Comun ~
        pPreUni = ITEM.PreUni.  /* Valor por defecto */ ~
        pLibre_c04 = "CD".          /* Valor por defecto */ ~
        /* % de Descuento */ ~
        pPorDto2 = (IF VtaDTabla.Libre_d01 > 0 THEN VtaDTabla.Libre_d01 ELSE VtaCTabla.Libre_d01). ~
        IF Vtadtabla.Tipo = "M" AND Vtadtabla.Libre_d02 > 0 THEN DO:~
            /* Caso especial: Tiene definido el Precio Unitario */~
            ASSIGN~
                pLibre_c04 = "UTILEX-ROJO"~
                pPorDto2 = 0~
                pPreUni = Vtadtabla.Libre_d02.~
        END.~
        ELSE IF pPorDto2 = 0 THEN NEXT. ~
        /* Solo productos sin promociones */ ~
        IF VtaCTabla.Libre_L02 = YES AND ~
            ( MAXIMUM( ITEM.Por_Dsctos[1], ITEM.Por_Dsctos[2], ITEM.Por_Dsctos[3] ) > 0 ~
              OR LOOKUP(ITEM.Libre_c04, 'PROM,VOL') > 0 ) ~
            THEN NEXT.~
        /* El mejor descuento */ ~
        IF VtaCTabla.Libre_L01 = YES AND ~
            MAXIMUM( ITEM.Por_Dsctos[1], ITEM.Por_Dsctos[2], ITEM.Por_Dsctos[3] ) > pPorDto2 ~
            THEN NEXT. ~
        /* Buscamos si es una excepción */ ~
        RUN Excepcion-Linea (OUTPUT pExcepcion). ~
        IF pExcepcion = YES THEN NEXT. ~
        ASSIGN ~
            ITEM.Por_Dsctos[1] = 0 ~
            ITEM.Por_Dsctos[2] = 0 ~
            ITEM.Por_Dsctos[3] = 0 ~
            ITEM.PreUni  = pPreUni ~
            ITEM.PorDto2 = pPorDto2 ~
            ITEM.Libre_c04 = pLibre_c04.  /* MARCA DESCUENTO POR ENCARTE */

    /* MARCAMOS LOS PRODUCTOS VALIDOS PARA EL ENCARTE */

    RLOOP:
    DO ON ERROR UNDO, LEAVE ON STOP UNDO, LEAVE:
        FOR EACH ITEM:
            /* Limpiamos controles */
            IF ITEM.PorDto2 > 0 OR LOOKUP(ITEM.Libre_c04, "CD,UTILEX-ROJO") > 0 THEN
                ASSIGN
                ITEM.PorDto2 = 0
                ITEM.Libre_c04 = "".
        END.

        /* ******************* POR ARTICULO ****************** */
        rloop1:
        FOR EACH Vtadtabla OF Vtactabla NO-LOCK WHERE Vtadtabla.Tipo = "M":
            FIND FIRST ITEM WHERE ITEM.codmat = Vtadtabla.LlaveDetalle 
                AND LOOKUP(ITEM.Libre_c04, "CD,UTILEX-ROJO") = 0 
                AND ITEM.Libre_c05 <> "OF"
                EXCLUSIVE-LOCK NO-ERROR.
            /* Verificamos el "Operador" */
            CASE TRUE:
                WHEN Vtactabla.Libre_c02 = "OR" THEN DO:    /* Por la compra de cualquiera de los productos */
                    IF NOT AVAILABLE ITEM THEN NEXT rloop1.
                END.
                WHEN Vtactabla.Libre_c02 = "AND" THEN DO:    /* Debe comprar todos los productos */
                    /* Debe comprarlo */
                    IF NOT AVAILABLE ITEM THEN UNDO rloop, LEAVE rloop.
                END.
            END CASE.
            {&Rutina-Comun}
        END.

        /* ******************* POR PROVEEDOR ****************** */
        rloop2:
        FOR EACH Vtadtabla OF Vtactabla NO-LOCK WHERE Vtadtabla.Tipo = "P":
            FIND FIRST ITEM WHERE LOOKUP(ITEM.Libre_c04, "CD,UTILEX-ROJO") = 0 
                AND ITEM.Libre_c05 <> "OF" 
                AND CAN-FIND(FIRST Almmmatg OF ITEM WHERE Almmmatg.codpr1 = Vtadtabla.LlaveDetalle NO-LOCK)
                NO-LOCK NO-ERROR.
            /* Verificamos el "Operador" */
            CASE TRUE:
                WHEN Vtactabla.Libre_c03 = "OR" THEN DO:    /* Por la compra de cualquiera de los productos */
                    IF NOT AVAILABLE ITEM THEN NEXT rloop2.
                END.
                WHEN Vtactabla.Libre_c03 = "AND" THEN DO:    /* Debe comprar todos los productos */
                    /* Debe comprarlo */
                    IF NOT AVAILABLE ITEM THEN UNDO rloop, LEAVE rloop.
                END.
            END CASE.
            FOR EACH ITEM WHERE LOOKUP(ITEM.Libre_c04, "CD,UTILEX-ROJO") = 0 AND
                ITEM.Libre_c05 <> "OF",
                FIRST Almmmatg OF ITEM WHERE Almmmatg.codpr1 = Vtadtabla.LlaveDetalle NO-LOCK:
                {&Rutina-Comun}
            END.
        END.

        /* ******************* POR LINEAS ****************** */
        rloop3:
        FOR EACH Vtadtabla OF Vtactabla NO-LOCK WHERE Vtadtabla.Tipo = "L":
            FIND FIRST ITEM WHERE LOOKUP(ITEM.Libre_c04, "CD,UTILEX-ROJO") = 0 
                AND ITEM.Libre_c05 <> "OF" 
                AND CAN-FIND(FIRST Almmmatg OF ITEM WHERE Almmmatg.codfam = Vtadtabla.LlaveDetalle AND 
                         (Vtadtabla.Libre_c01 = "" OR Almmmatg.subfam = Vtadtabla.Libre_c01) NO-LOCK)
                NO-LOCK NO-ERROR.
            /* Verificamos el "Operador" */
            CASE TRUE:
                WHEN Vtactabla.Libre_c05 = "OR" THEN DO:    /* Por la compra de cualquiera de los productos */
                    IF NOT AVAILABLE ITEM THEN NEXT rloop3.
                END.
                WHEN Vtactabla.Libre_c05 = "AND" THEN DO:    /* Debe comprar todos los productos */
                    /* Debe comprarlo */
                    IF NOT AVAILABLE ITEM THEN UNDO rloop, LEAVE rloop.
                END.
            END CASE.
            FOR EACH ITEM WHERE LOOKUP(ITEM.Libre_c04, "CD,UTILEX-ROJO") = 0 
                AND ITEM.Libre_c05 <> "OF",
                FIRST Almmmatg OF ITEM NO-LOCK WHERE Almmmatg.codfam = Vtadtabla.LlaveDetalle AND 
                (Vtadtabla.Libre_c01 = "" OR Almmmatg.subfam = Vtadtabla.Libre_c01):
                {&Rutina-Comun}
            END.
        END.

    END.

    /* CALCULO FINAL */
    x-ImpDto2 = 0.
    FOR EACH ITEM WHERE LOOKUP(ITEM.Libre_c04, "CD,UTILEX-ROJO") > 0, 
        FIRST Almmmatg OF ITEM NO-LOCK:
        ASSIGN
            ITEM.ImpLin = ITEM.CanPed * ITEM.PreUni * 
            ( 1 - ITEM.Por_Dsctos[1] / 100 ) *
            ( 1 - ITEM.Por_Dsctos[2] / 100 ) *
            ( 1 - ITEM.Por_Dsctos[3] / 100 )
            ITEM.ImpDto2 = ROUND ( ITEM.ImpLin * ITEM.PorDto2 / 100, 2).
        IF ITEM.Por_Dsctos[1] = 0 AND ITEM.Por_Dsctos[2] = 0 AND ITEM.Por_Dsctos[3] = 0 
            THEN ITEM.ImpDto = 0.
        ELSE ITEM.ImpDto = ITEM.CanPed * ITEM.PreUni - ITEM.ImpLin.
        ASSIGN
            ITEM.ImpLin = ROUND(ITEM.ImpLin, 2)
            ITEM.ImpDto = ROUND(ITEM.ImpDto, 2).
        IF ITEM.AftIsc 
            THEN ITEM.ImpIsc = ROUND(ITEM.PreBas * ITEM.CanPed * (Almmmatg.PorIsc / 100),4).
        IF ITEM.AftIgv 
            THEN ITEM.ImpIgv = ITEM.ImpLin - ROUND( ITEM.ImpLin  / ( 1 + (Ccbcdocu.PorIgv / 100) ), 4 ).
        ASSIGN
            x-ImpDto2 = x-ImpDto2 + ITEM.ImpDto2.
    END.

    /* CALCULO FINAL */
    ASSIGN
        x-ImpDto2 = 0.
    FOR EACH ITEM WHERE LOOKUP(ITEM.Libre_c04, "CD") > 0:
        x-ImpDto2 = x-ImpDto2 + ITEM.ImpDto2.
    END.
    IF VtaCTabla.Libre_d02 > 0 THEN DO:
        DEF VAR y-ImpDto2 LIKE Faccpedi.ImpDto2 NO-UNDO.
        y-ImpDto2 = x-ImpDto2.
        x-ImpDto2 = MINIMUM(x-ImpDto2, VtaCTabla.Libre_d02).
        IF y-ImpDto2 <> x-ImpDto2 THEN DO:
            FOR EACH ITEM WHERE ITEM.PorDto2 > 0:
                ITEM.ImpDto2 = ROUND (ITEM.ImpDto2 * ( x-ImpDto2 / y-ImpDto2 ), 2).
            END.
        END.
    END.


    /* FINALES */
      ASSIGN
          x-ImpDto2 = 0
          FILL-IN-NewImport = 0.

      FOR EACH ITEM NO-LOCK: 
          x-ImpDto2 = x-ImpDto2 + ITEM.ImpDto2.
          FILL-IN-NewImport = FILL-IN-NewImport + ITEM.ImpLin.
      END.
      /* RHC 06/05/2014 En caso tenga descuento por Encarte */
      IF x-ImpDto2 > 0 THEN DO:
          ASSIGN
              FILL-IN-NewImport = FILL-IN-NewImport - x-ImpDto2.
      END.


END PROCEDURE.


PROCEDURE Excepcion-Linea:

    DEF OUTPUT PARAMETER pExcepcion AS LOG.

    pExcepcion = NO.

    /* Por Linea y/o Sublinea */
    FIND B-TABLA OF Vtactabla WHERE B-TABLA.LlaveDetalle = Almmmatg.codfam
        AND B-TABLA.Libre_c01 = Almmmatg.subfam
        AND B-TABLA.Tipo = "XL"
        NO-LOCK NO-ERROR.
    IF AVAILABLE B-TABLA THEN pExcepcion = YES.
    FIND B-TABLA OF Vtactabla WHERE B-TABLA.LlaveDetalle = Almmmatg.codfam
        AND B-TABLA.Libre_c01 = ""
        AND B-TABLA.Tipo = "XL"
        NO-LOCK NO-ERROR.
    IF AVAILABLE B-TABLA THEN pExcepcion = YES.
    /* Por Producto */
    FIND B-TABLA OF Vtactabla WHERE B-TABLA.LlaveDetalle = Almmmatg.codmat
        AND B-TABLA.Tipo = "XM"
        NO-LOCK NO-ERROR.
    IF AVAILABLE B-TABLA THEN pExcepcion = YES.

END PROCEDURE.


