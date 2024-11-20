/* migrar utilex */
DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-codalm AS CHAR INIT '511' NO-UNDO.

FOR EACH almmmate NO-LOCK WHERE almmmate.codcia = s-codcia
    AND almmmate.codalm = s-codalm,
    FIRST almmmatg OF almmmate NO-LOCK WHERE LOOKUP(TRIM(almmmatg.tpomrg),',2') > 0
    BY almmmate.codmat:
    DISPLAY almmmate.codmat. PAUSE 0.
    RUN replica.
END.

PROCEDURE replica:

    DEF VAR X-CanalVenta AS CHAR INIT "MIN" NO-UNDO.
    FIND FIRST almacen OF almmmate NO-LOCK NO-ERROR.
    IF AVAILABLE almacen THEN DO:
        FIND gn-divi WHERE gn-divi.codcia = almacen.codcia AND gn-divi.coddiv = almacen.coddiv NO-LOCK NO-ERROR.
        IF AVAILABLE gn-divi THEN x-CanalVenta = gn-divi.CanalVenta.
        {rpl/reptrig.i
        &Table  = almmmate
        &Key    =  "string(almmmate.codcia,'999') + string(almmmate.codalm,'x(5)') + string(almmmate.codmat,'x(6)')"
        &Prg    = r-almmmate
        &Event  = WRITE
        &FlgDB0 = TRUE
        &FlgDB1 = "NOT LOOKUP(almacen.coddiv, '00501') > 0"  /* TIENDA CONO NORTE */
        &FlgDB2 = "NOT LOOKUP(almacen.coddiv, '00023') > 0"  /* TIENDA SURQUILLO */
        &FlgDB3 = "NOT LOOKUP(almacen.coddiv, '00027') > 0"  /* TIENDA CHORRILLOS */
        &FlgDB4 = "NOT LOOKUP(almacen.coddiv, '00502') > 0"  /* TIENDA SAN BORJA */
        &FlgDB5 = "NOT LOOKUP(almacen.coddiv, '00503') > 0"  /* TIENDA LA MOLINA */
        &FlgDB6 = "NOT LOOKUP(almacen.coddiv, '00504') > 0"  /* TIENDA BENEFICENCIA */
        &FlgDB7 = "NOT LOOKUP(almacen.coddiv, '00505') > 0"  /* TIENDA PLAZA NORTE */
        &FlgDB8 = "NOT LOOKUP(almacen.coddiv, '00507') > 0"  /* TIENDA LA RAMBLA */
        &FlgDB9 = "NOT LOOKUP(almacen.coddiv, '00508') > 0"  /* TIENDA SAN ISIDRO */
        &FlgDB10 = "NOT LOOKUP(almacen.coddiv, '00065') > 0"  /* TIENDA CHICLAYO */
        &FlgDB11 = "NOT LOOKUP(almacen.coddiv, '00510') > 0"  /* TIENDA ATOCONGO */
        &FlgDB12 = "NOT LOOKUP(almacen.coddiv, '00511') > 0"  /* TIENDA ANGAMOS  */
        &FlgDB13 = "NOT LOOKUP(almacen.coddiv, '00512') > 0"  /* TIENDA SALAVERRY  */
        &FlgDB14 = "NOT LOOKUP(almacen.coddiv, '00513') > 0"  /* TIENDA CENTRO CIVICO  */
        &FlgDB15 = "NOT LOOKUP(almacen.coddiv, '00514') > 0"  /* TIENDA PRIMAVERA  */
        &FlgDB16 = "NOT LOOKUP(almacen.coddiv, '00516') > 0"  /* TIENDA BELLAVISTA  */
        &FlgDB17 = TRUE
        &FlgDB18 = "NOT LOOKUP(almacen.coddiv,'00060,00061,00062,00063,10060') > 0"     /* AREQUIPA */
        &FlgDB19 = "NOT LOOKUP(almacen.coddiv, '00069') > 0"  /* TRUJILLO */
        &FlgDB20 = "NOT x-CanalVenta = 'MIN'"   /*"NOT LOOKUP(almacen.coddiv, '00023,00027,00501,00502,00503,00504,00510') > 0"    /* SERVIDOR UTILEX */*/
        &FlgDB30 = NO
        }
    END.
END PROCEDURE.

