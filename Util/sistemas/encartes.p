
OUTPUT TO d:\encartes.txt.
PUT UNFORMATTED
    'ENCARTE|ESTADO|DESCRIPCION|DESDE|HASTA|USR CREACION|FCH CREACION|'
    'USR MODIFICACION|FCH MODIFICACION|% DESCTO|TOPE DCTO S/.|'
    'MIN VTA S/.|APLICA MEJOR DCTO|SOLO SIN PROM|PIDE DNI|'
    'COD DE TRABAJADOR|ACUMULA DCTOS|KIT PROMOCIONAL|TOPE'
    SKIP.
FOR EACH vtactabla NO-LOCK WHERE codcia = 1
    AND tabla = "UTILEX-ENCARTE":
    PUT UNFORMATTED
        llave               '|'
        estado              '|'
        descripcion         '|'
        fechainicial        '|'
        fechafinal          '|'
        usrcreacion         '|'
        fchcreacion         '|'
        usrmodificacion     '|'
        fchmodificacion     '|'
        libre_d01           '|'
        libre_d02           '|'
        libre_d04           '|'
        libre_l01           '|'
        libre_l02           '|'
        libre_l03           '|'
        libre_l04           '|'
        libre_l05           '|'
        libre_c01           '|'
        libre_d03
        SKIP.
END.
OUTPUT CLOSE.

OUTPUT TO d:\articulos.txt.
PUT UNFORMATTED 
    'ENCARTE|ARTICULO|DESCRIPCION|UNIDAD|% DCT|P.U. S/.'
    SKIP.
FOR EACH vtactabla NO-LOCK WHERE codcia = 1 AND tabla = "UTILEX-ENCARTE",
    EACH VtaDTabla OF VtaCTabla NO-LOCK WHERE VtaDTabla.Tipo = "M",
    FIRST Almmmatg WHERE Almmmatg.CodCia = VtaDTabla.CodCia
        AND Almmmatg.codmat = VtaDTabla.LlaveDetalle NO-LOCK:
    PUT UNFORMATTED
        VtaCTabla.llave        '|'
        VtaDTabla.LlaveDetalle '|'
        Almmmatg.DesMat        '|'
        Almmmatg.UndBas        '|'
        VtaDTabla.Libre_d01    '|'
        VtaDTabla.Libre_d02
        SKIP.
END.
OUTPUT CLOSE.

OUTPUT TO d:\proveedores.txt.
PUT UNFORMATTED 
    'ENCARTE|PROVEEDOR|NOMBRE|% DCTO'
    SKIP.
FOR EACH vtactabla NO-LOCK WHERE codcia = 1 AND tabla = "UTILEX-ENCARTE",
    EACH Vtadtabla OF Vtactabla WHERE VtaDTabla.Tipo = "P" NO-LOCK,
    FIRST gn-prov WHERE gn-prov.CodPro = VtaDTabla.LlaveDetalle
        AND gn-prov.CodCia = 000 NO-LOCK:
    PUT UNFORMATTED
        VtaCTabla.llave        '|'
        VtaDTabla.LlaveDetalle '|'
        gn-prov.nompro         '|'
        VtaDTabla.Libre_d01    
        SKIP.
END.
OUTPUT CLOSE.

OUTPUT TO d:\lineas.txt.
PUT UNFORMATTED 
    'ENCARTE|LINEA|DESCRIPCION|SUBLINEA|DESCRIPCION|% DCTO'
    SKIP.
FOR EACH vtactabla NO-LOCK WHERE codcia = 1 AND tabla = "UTILEX-ENCARTE",
    EACH VtaDTabla OF VtaCTabla WHERE VtaDTabla.Tipo = "L" NO-LOCK,
    FIRST Almtfami WHERE Almtfami.CodCia = VtaDTabla.CodCia 
        AND Almtfami.codfam = VtaDTabla.LlaveDetalle NO-LOCK:
    FIND FIRST AlmSFami WHERE AlmSFami.CodCia = VtaDTabla.CodCia
        AND AlmSFami.codfam = VtaDTabla.LlaveDetalle
        AND AlmSFami.subfam = VtaDTabla.Libre_c01
        NO-LOCK NO-ERROR.
    IF AVAILABLE Almsfami THEN
        PUT UNFORMATTED
        VtaCTabla.llave        '|'
        VtaDTabla.LlaveDetalle  '|'
        Almtfami.desfam         '|'
        VtaDTabla.Libre_c01     '|'
        AlmSFami.dessub         '|'
        VtaDTabla.Libre_d01    
        SKIP.
    ELSE PUT UNFORMATTED
        VtaCTabla.llave        '|'
        VtaDTabla.LlaveDetalle  '|'
        Almtfami.desfam         '|'
        '|'
        '|'
        VtaDTabla.Libre_d01    
        SKIP.
END.
OUTPUT CLOSE.

OUTPUT TO d:\lineas-excepciones.txt.
PUT UNFORMATTED 
    'ENCARTE|LINEA|DESCRIPCION|SUBLINEA|DESCRIPCION'
    SKIP.
FOR EACH vtactabla NO-LOCK WHERE codcia = 1 AND tabla = "UTILEX-ENCARTE",
    EACH VtaDTabla OF VtaCTabla WHERE VtaDTabla.Tipo = "XL" NO-LOCK,
    FIRST Almtfami WHERE Almtfami.CodCia = VtaDTabla.CodCia 
        AND Almtfami.codfam = VtaDTabla.LlaveDetalle NO-LOCK:
    FIND FIRST AlmSFami WHERE AlmSFami.CodCia = VtaDTabla.CodCia
        AND AlmSFami.codfam = VtaDTabla.LlaveDetalle
        AND AlmSFami.subfam = VtaDTabla.Libre_c01
        NO-LOCK NO-ERROR.
    IF AVAILABLE Almsfami THEN
        PUT UNFORMATTED
        VtaCTabla.llave        '|'
        VtaDTabla.LlaveDetalle  '|'
        Almtfami.desfam         '|'
        VtaDTabla.Libre_c01     '|'
        AlmSFami.dessub         
        SKIP.
    ELSE PUT UNFORMATTED
        VtaCTabla.llave        '|'
        VtaDTabla.LlaveDetalle  '|'
        Almtfami.desfam         '|'
        '|'
        SKIP.
END.
OUTPUT CLOSE.

OUTPUT TO d:\articulos-excepciones.txt.
PUT UNFORMATTED 
    'ENCARTE|ARTICULO|DESCRIPCION|UNIDAD'
    SKIP.
FOR EACH vtactabla NO-LOCK WHERE codcia = 1 AND tabla = "UTILEX-ENCARTE",
    EACH VtaDTabla OF VtaCTabla NO-LOCK WHERE VtaDTabla.Tipo = "XM",
    FIRST Almmmatg WHERE Almmmatg.CodCia = VtaDTabla.CodCia
        AND Almmmatg.codmat = VtaDTabla.LlaveDetalle NO-LOCK:
    PUT UNFORMATTED
        VtaCTabla.llave        '|'
        VtaDTabla.LlaveDetalle '|'
        Almmmatg.DesMat        '|'
        Almmmatg.UndBas        
        SKIP.
END.
OUTPUT CLOSE.

OUTPUT TO d:\clientes-asociados.txt.
PUT UNFORMATTED 
    'ENCARTE|CLIENTE|NOMBRE'
    SKIP.
FOR EACH vtactabla NO-LOCK WHERE codcia = 1 AND tabla = "UTILEX-ENCARTE",
    EACH VtaDTabla OF VtaCTabla NO-LOCK WHERE VtaDTabla.Tipo = "CA":
    PUT UNFORMATTED
        VtaCTabla.llave        '|'
        VtaDTabla.LlaveDetalle '|'
        VtaDTabla.Libre_c01    
        SKIP.
END.
OUTPUT CLOSE.

