	��V�Bf 8  ? �                                              �J 382000EFutf-8 MAIN D:\newsie\on_in_co\aplic\alm\dmate-matg-p2.w,, PROCEDURE initializeObject,, PROCEDURE disable_UI,, PROCEDURE serverCommit,,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE remoteCommit,,INPUT-OUTPUT pcContext CHARACTER,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE pushTableAndValidate,,INPUT pcValType CHARACTER,INPUT-OUTPUT RowObjUpd TABLE PROCEDURE pushRowObjUpdTable,,INPUT RowObjUpd TABLE PROCEDURE initProps,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE viewObject,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE disconnectObject,, PROCEDURE destroyObject,, PROCEDURE bindServer,, PROCEDURE transferDBRow,,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER PROCEDURE startFilter,, PROCEDURE releaseDBRow,, PROCEDURE refetchDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE filterContainerHandler,,INPUT phFilterContainer HANDLE PROCEDURE fetchDBRowForUpdate,, PROCEDURE confirmContinue,,INPUT-OUTPUT pioCancel LOGICAL PROCEDURE compareDBRow,, PROCEDURE bufferCopyDBToRO,,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER PROCEDURE assignDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE updateState,,INPUT pcState CHARACTER PROCEDURE updateQueryPosition,, PROCEDURE updateAddQueryWhere,,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER PROCEDURE undoTransaction,, PROCEDURE transferToExcel,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE synchronizeProperties,,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER PROCEDURE submitValidation,,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER PROCEDURE submitForeignKey,,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER PROCEDURE submitCommit,,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL PROCEDURE startServerObject,, PROCEDURE setPropertyList,,INPUT pcProperties CHARACTER PROCEDURE serverFetchRowObjUpdTable,,OUTPUT phRowObjUpd TABLE-HANDLE PROCEDURE serverSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE PROCEDURE saveContextAndDestroy,,OUTPUT pcContext CHARACTER PROCEDURE rowObjectState,,INPUT pcState CHARACTER PROCEDURE retrieveFilter,, PROCEDURE restartServerObject,, PROCEDURE remoteSendRows,,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE,OUTPUT pocMessages CHARACTER PROCEDURE refreshRow,, PROCEDURE printToCrystal,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE isUpdatePending,,INPUT-OUTPUT plUpdate LOGICAL PROCEDURE initializeServerObject,, PROCEDURE home,, PROCEDURE genContextList,,OUTPUT pcContext CHARACTER PROCEDURE fetchPrev,, PROCEDURE fetchNext,, PROCEDURE fetchLast,, PROCEDURE fetchFirst,, PROCEDURE fetchBatch,,INPUT plForwards LOGICAL PROCEDURE endClientDataRequest,, PROCEDURE destroyServerObject,, PROCEDURE describeSchema,,INPUT pcSdoName CHARACTER,OUTPUT hTtSchema TABLE-HANDLE PROCEDURE dataAvailable,,INPUT pcRelative CHARACTER PROCEDURE copyColumns,,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE PROCEDURE commitTransaction,, PROCEDURE clientSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER PROCEDURE batchServices,,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER FUNCTION deleteRecordStatic,logical,INPUT piTableIndex INTEGER FUNCTION getRowObjUpdStatic,widget-handle, FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION whereClauseBuffer,CHARACTER,INPUT pcWhere CHARACTER FUNCTION setQueryWhere,LOGICAL,INPUT pcWhere CHARACTER FUNCTION setQueryString,LOGICAL,INPUT pcQueryString CHARACTER FUNCTION setQuerySort,LOGICAL,INPUT pcSort CHARACTER FUNCTION setQueryPosition,LOGICAL,INPUT pcPosition CHARACTER FUNCTION rowidWhereCols,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION rowidWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION resolveBuffer,CHARACTER,INPUT pcBuffer CHARACTER FUNCTION removeQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER FUNCTION removeForeignKey,LOGICAL, FUNCTION refreshRowident,CHARACTER,INPUT pcRowident CHARACTER FUNCTION newWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION newQueryValidate,CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryString,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER FUNCTION insertExpression,CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER FUNCTION indexInformation,CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER FUNCTION getTargetProcedure,HANDLE, FUNCTION getQueryWhere,CHARACTER, FUNCTION getQueryString,CHARACTER, FUNCTION getQuerySort,CHARACTER, FUNCTION getQueryPosition,CHARACTER, FUNCTION getForeignValues,CHARACTER, FUNCTION getDataColumns,CHARACTER, FUNCTION excludeColumns,CHARACTER,INPUT iTable INTEGER FUNCTION dbColumnHandle,HANDLE,INPUT pcColumn CHARACTER FUNCTION dbColumnDataName,CHARACTER,INPUT pcDbColumn CHARACTER FUNCTION columnValMsg,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnTable,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnQuerySelection,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDbColumn,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDataType,CHARACTER,INPUT pcColumn CHARACTER FUNCTION bufferWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER FUNCTION bufferCompareDBToRO,LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER FUNCTION assignQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION addQueryWhere,LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION getObjectType,character, FUNCTION updateRow,LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER FUNCTION submitRow,LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER FUNCTION rowValues,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION rowAvailable,LOGICAL,INPUT pcDirection CHARACTER FUNCTION prepareQuery,LOGICAL,INPUT pcQuery CHARACTER FUNCTION openQuery,LOGICAL, FUNCTION openDataQuery,LOGICAL,INPUT pcPosition CHARACTER FUNCTION hasForeignKeyChanged,LOGICAL, FUNCTION getLastCommitErrorType,CHARACTER, FUNCTION firstRowIds,CHARACTER,INPUT pcQueryString CHARACTER FUNCTION findRowWhere,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION findRow,LOGICAL,INPUT pcKeyValues CHARACTER FUNCTION fetchRowIdent,CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER FUNCTION fetchRow,CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER FUNCTION deleteRow,LOGICAL,INPUT pcRowIdent CHARACTER FUNCTION createRow,LOGICAL,INPUT pcValueList CHARACTER FUNCTION copyRow,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION colValues,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION columnProps,CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER FUNCTION closeQuery,LOGICAL, FUNCTION canNavigate,LOGICAL, FUNCTION cancelRow,CHARACTER, FUNCTION addRow,CHARACTER,INPUT pcViewColList CHARACTER TEMP-TABLE RowObjUpd 0,RowNum:RowNum 0 NO,AlmDes character 0 0,CodAlm character 1 0,CodCia integer 2 0,codmat character 3 0,CodUbi character 4 0,StkAct decimal 5 0,CodCia-2 integer 6 0,codfam character 7 0,CodMar character 8 0,codmat-2 character 9 0,CodPr1 character 10 0,DesMar character 11 0,DesMat character 12 0,TpoArt character 13 0,UndStk character 14 0,subfam character 15 0,FchIng date 16 0,Pesmat decimal 17 0,RowNum integer 18 0,RowIdent character 19 0,RowMod character 20 0,RowIdentIdx character 21 0,RowUserProp character 22 0,ChangedFields character 23 0      <:              `'             	 <:  $�              �              tA  	   +   H� �  W   � `  X   H� �  Y   �   [   �   \   0� <  ]   l�    ^   �� 0  `   �� 0  a   ? �� #!  iSO8859-1                                                                           |9   " �                                      �                   ��   	             �9  �    (    FG   T�              ��  �   :      :          �                                             PROGRESS                         �           
    
                    �              �                                                                                                     
                                                                                                            ��         �             �                                                                                             ��                      INTEGRAL                         PROGRESS                         \     "   �      "                          _�xc            +   �                              �  d                      �  t  P�     CODMATDESMATCODMARUNDSTKUNDCMPFACEQUCODCTACODNEWMONVTAPREVTAPREBASAFTIGVVINMN1CODCIAVINMN2CODFAMVCTMN1FCHACTCODPR1CODPR2VCTMN2ARTPROFCHUSALFCHUCMPPMAXMN1PMAXMN2PULTMN1PULTMN2USUARIOFCHINGFCHCESFCHALZCLFMATUNDBASSUBFAMCODBRRCODANTTIPARTFCHPRMDFCHPRMHFCHREAPESMATDETALLECANEMPALMACENESDESMARAFTISCPORISCPORVTATPOMRGCTOLISCTOPRMMRGUTIPORMAXFCHMPREUNDANTPREANTPREACTDSCTOSTPOPROPORIGVCTOTOTTPOSUMCTOUNDORDENORDLISORDTMPTPOARTTPOCMBPPCHR__01CHR__02CHR__03DEC__01DEC__02DEC__03DATE__01DATE__02DATE__03MRGUTI-AMRGUTI-BMRGUTI-CPREOFIUNDAUNDBUNDCFLGINTFLGPRECLASEFCHPROMCATCONTATIPROTDSCTOPROMINFORFLGINFORPROMDIVIPROMFCHDPROMFCHHPROMDTODTOVOLRDTOVOLDUNDALTDSCALTMRGALTPREALTLICENCIAPROMMINDIVIPROMMINFCHDPROMMINFCHHPROMMINDTOCODDIGESAVTODIGESALIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02STKMINSTKMAXSTKREPDESCRIPCION-LARGADESCRIPCION-TECNICASW-WEBWEB-SUBCATEGORIALIBRE_D03LIBRE_D04LIBRE_D05PESOBRUTOPAQUETELARGOALTOANCHOCTOLISMARCOCTOTOTMARCOCODSSFAMCLFESPECIALFLGCOMERCIALLIBRE_C06LIBRE_C07LIBRE_C08LIBRE_C09LIBRE_C10CODIGOPADREFACTORPADREREQUIERESERIALNRREQUIEREDUEDATEDTOVOLPTASAIMPUESTOIMPORTEUNITARIOSINIMPUESTODTOVOLPSINIMPUESTOIMPORTEUNITARIOSINIMPUESTO_AIMPORTEUNITARIOSINIMPUESTO_BIMPORTEUNITARIOSINIMPUESTO_CDTOVOLPIMPUESTOIMPORTEUNITARIOIMPUESTOIMPORTEUNITARIOIMPUESTO_AIMPORTEUNITARIOIMPUESTO_BIMPORTEUNITARIOIMPUESTO_C                                                                      	          
                                                                                                                                                                                                                                      !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2         3          4          5         6          7          8         9          :          ;          <         =          >          ?          @          A          B          C          D          E          F          G          H          I          J          K          L          M          N          O          P          Q          R          S          T          U          V          W          X          Y          Z          [         \         ]         ^         _         `         a 
        b 
        c 
        d 
        e 
        f 
        g         h         i         j         k         l 
        m 
        n 
        o 
        p          q          r          s          t          u          v          w          x          y          z          {          |          }          ~                    �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          � 
        �          �          � 
        �          �          �          � 
        �          �          �          �          �     B   �      B                          �.�d            B   .'                              �  �                      �  �  %     CODCIACODALMCODMATUNDVTACODUBISTKACTSTKMINSTKMAXSTKREPSTKINIVINMN1VINMN2VCTMN1VCTMN2FCHINGFCHSALFCHINVSELINVFACEQUDESMATALMDESCODMARCODANTSTKACTCBDLIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02STKCOMPROMETIDOSTOCKMAXSTOCKSEGSTOCKMAXSEG                                                                        	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          d  .      �  
    
                  �  �             P                                                                                          .          
    @      �  
    
                  x  @             �                                                                                          @          
  �  R      8  
    
                  $  �  	           �                                                                                          R          
  h  _      �  
    
                  �  �  
           T                                                                                          _          
    r      �  
    
                  |  D                                                                                                        r          
  �  �      <  
    
                  (  �             �                                                                                          �          
  l  �      �  
    
                  �  �             X                                                                                          �          
    �      �  
    
                  �  H                                                                                                       �          
  �  �      @                         ,  �             �                                                                                          �            p  �      �                        �  �             \                                                                                          �              �      �  
    
                  �  L                                                                                                       �          
  �  �      D  
    
                  0  �             �                                                                                          �          
  t  �      �  
    
                  �  �             `                                                                                          �          
           �                        �  P                                                                                                                   �        H                        4  �             �                                                                                                      x        �                        �  �             d                                                                                                          .      �                        �  $                                                                                                       .            �         �       �  X  �,  .   �,  �  @I      4-         �             "          �#      �              �       �  X  �8  /   �8  �  ��      9         �         �    �-          �/      �                 H�                                               L�          |!  �!  L l\                 �         
             
             
                                         
                                                                                                               
             
                                          L   \   l   |   �   �   �   �   �   �   �   �       ,  <  L  \      L   \   l   |   �   �   �   �   �   �   �   �      ,  <  L  \                                                                                                                                     	                  
                                                                                                                                                                                                                                                                                             (  (  $(  8(                             <(  D(  L(  \(  T(          `(             t(  |(  �(  �(  �(          �(             �(  �(  �(  �(  �(                         �(  �(  �(  �(  �(           )             )  )  0)  P)  @)          T)             d)  p)  t)  |)  x)          �)             �)  �)  �)  �)  �)          �)             �)  �)  �)  �)  �)                         �)  *  *  ,*  *                         0*  8*  @*  h*  T*          l*             �*  �*  �*  �*  �*                         �*  �*  �*  �*  �*          �*             �*  +  +  +  +                          +  (+  0+  P+  @+          T+             d+  l+  t+  �+  �+                         �+  �+  �+  �+  �+          �+             �+  �+  �+  �+  �+                          ,  ,  ,  ,                              ,  ,,  4,  @,                              D,  L,  T,  \,                             `,  l,  t,  �,                             �,  �,  �,  �,                                                                          AlmDes  X(70)   Almacen Despacho        CodAlm  x(5)    Almac�n Almac�n     C�digo de almac�n   CodCia  999 Cia Cia 0   C�digo de compa�ia  codmat  X(6)    Codigo Articulo Codigo!Articulo     CodUbi  x(7)    Ubicaci�n   Ubicaci�n       C�digo de ubicaci�n StkAct  (ZZZ,ZZZ,ZZ9.99)    Stock actual    Stock actual    0   Stock actual    CodCia-2    999 Cia Cia 0   C�digo de compa�ia  codfam  X(3)    Familia C�digo!familia      Codigo de familia   CodMar  X(4)    Marca   Marca       codmat-2    X(6)    Codigo Articulo Codigo Articulo     CodPr1  x(11)   Proveedor principal Proveedor!principal     Proveedor principal DesMar  X(30)   Des.Marca   Descripcion!Marca       DesMat  X(60)   Descripci�n Descripci�n     Descripci�n del material    TpoArt  X(1)    Estado  Estado  A   UndStk  X(8)    Unidad de stock Unidad!stock        Unidad de stock subfam  X(3)    Sub-Familia Sub!Familia     FchIng  99/99/9999  Fecha Ing.  Fecha Ing.  ?   Fecha de Ingreso    Pesmat  ->>,>>9.9999    Peso    Peso    0   RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     �   �  ���������             A  �            �        �        �                �     i     i     i     	 	 	      $  +  2  9  @  G  P  W  ^  g  n  u  |  �  �  �  �  �  �  �  �  �                                                                                                                                     	                  
                                                                                                                                                                                                                                                                                                               �3  �3  �3  �3                             �3   4  4  4  4          4             04  84  <4  D4  @4          H4             \4  d4  l4  �4  |4                         �4  �4  �4  �4  �4          �4             �4  �4  �4  5  �4          5              5  ,5  05  85  45          <5             P5  X5  `5  x5  h5          |5             �5  �5  �5  �5  �5                         �5  �5  �5  �5  �5                         �5  �5  �5  $6  6          (6             <6  D6  L6  l6  X6                         p6  x6  �6  �6  �6          �6             �6  �6  �6  �6  �6                         �6  �6  �6  7  �6          7              7  (7  07  H7  <7                         L7  T7  `7  x7  l7          |7             �7  �7  �7  �7  �7                         �7  �7  �7  �7                             �7  �7  �7  �7                               8  8  8  8                             8  (8  08  <8                             @8  L8  T8  `8                              d8  t8  |8  �8                                                                          AlmDes  X(70)   Almacen Despacho        CodAlm  x(5)    Almac�n Almac�n     C�digo de almac�n   CodCia  999 Cia Cia 0   C�digo de compa�ia  codmat  X(6)    Codigo Articulo Codigo!Articulo     CodUbi  x(7)    Ubicaci�n   Ubicaci�n       C�digo de ubicaci�n StkAct  (ZZZ,ZZZ,ZZ9.99)    Stock actual    Stock actual    0   Stock actual    CodCia-2    999 Cia Cia 0   C�digo de compa�ia  codfam  X(3)    Familia C�digo!familia      Codigo de familia   CodMar  X(4)    Marca   Marca       codmat-2    X(6)    Codigo Articulo Codigo Articulo     CodPr1  x(11)   Proveedor principal Proveedor!principal     Proveedor principal DesMar  X(30)   Des.Marca   Descripcion!Marca       DesMat  X(60)   Descripci�n Descripci�n     Descripci�n del material    TpoArt  X(1)    Estado  Estado  A   UndStk  X(8)    Unidad de stock Unidad!stock        Unidad de stock subfam  X(3)    Sub-Familia Sub!Familia     FchIng  99/99/9999  Fecha Ing.  Fecha Ing.  ?   Fecha de Ingreso    Pesmat  ->>,>>9.9999    Peso    Peso    0   RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     ChangedFields   x(8)    ChangedFields       �   �  ���������             A  �            �        �        �                �     i     i     i     	 	 	      $  +  2  9  @  G  P  W  ^  g  n  u  |  �  �  �  �  �  �  �  �  �  �    ��                            ����                            �    l�                    ��    !   h�                    g�    undefined                                                               �       p�  �   l   ��  ��                    �����               p�r                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D       �     9          ��    �   �  4      d       4   ����d                 D                      ��                  �   �                   ��r                       �   �  �  	  �   x                                        3   ����|       O   �   ��  ��  �   batchServices                               4        ��                  W  Z  L              ��r                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             d               ��                  �           ��                            ����                            clientSendRows                              �  p      ��                  \  b  �              �r                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                �               ��   <                            ��   d             0               ��                  X           ��                            ����                            commitTransaction                               X  @      ��                  d  e  p              \*s                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            copyColumns                             X  @      ��                  g  j  p              �,s                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               �� 
                 �  
         ��                            ����                            dataAvailable                               �  �      ��                  l  n  �              x�o                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            describeSchema                              �	  �	      ��                  p  s  �	              ��o                    O   ����    e�          O   ����    R�          O   ����    ��            ��   <
             
               �� 
          �       0
  
         ��                            ����                            destroyServerObject                             0        ��                  u  v  H              ��o                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            endClientDataRequest                                <  $      ��                  x  y  T              �o                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchBatch                              <  $      ��                  {  }  T              ��o                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  l           ��                            ����                            fetchFirst                              d  L      ��                    �  |              ��o                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               d  L      ��                  �  �  |              �o                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               d  L      ��                  �  �  |              ��o                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               d  L      ��                  �  �  |              Фo                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            genContextList                              h  P      ��                  �  �  �              H�o                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            home                                �  t      ��                  �  �  �              ��o                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �  |      ��                  �  �  �              بo                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �  �      ��                  �  �  �              <�o                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            isUpdatePending                             �  �      ��                  �  �  �              �o                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            printToCrystal                              �  �      ��                  �  �  �               �o                    O   ����    e�          O   ����    R�          O   ����    ��            ��   4                             ��   \             (               ��                  P           ��                            ����                            refreshRow                              H  0      ��                  �  �  `              �cp                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            remoteSendRows                              L  4      ��                  �  �  d              �rp                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             |               ��   �             �               ��                 �               ��   (             �               ��   P                            ��   x             D               �� 
  �      �       l  
             ��                  �           ��                            ����                            restartServerObject                             �  |      ��                  �  �  �              �p                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            retrieveFilter                              �  �      ��                  �  �  �              �p                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            rowObjectState                              �  �      ��                  �  �  �              �%p                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            saveContextAndDestroy                               �  �      ��                  �  �  �              `�p                    O   ����    e�          O   ����    R�          O   ����    ��            ��                               ��                            ����                            serverSendRows                              �   �       ��                  �  �  !              8q                    O   ����    e�          O   ����    R�          O   ����    ��            ��   `!             ,!               ��   �!             T!               ��   �!             |!               ��   �!             �!               ��    "             �!               �� 
          �       �!  
         ��                            ����                            serverFetchRowObjUpdTable                               �"  �"      ��                  �  �  #              ��q                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
          �       ,#  
         ��                            ����                            setPropertyList                             ($  $      ��                  �  �  @$              \�q                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  X$           ��                            ����                            serverSendRows                              T%  <%      ��                  �  �  l%              �zq                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �%             �%               ��   �%             �%               ��   &             �%               ��   0&             �%               ��   X&             $&               �� 
          �       L&  
         ��                            ����                            startServerObject                               L'  4'      ��                  �  �  d'              �Z                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            submitCommit                                P(  8(      ��                  �  �  h(              $-                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �(             �(               ��                  �(           ��                            ����                            submitForeignKey                                �)  �)      ��                  �  �  �)              �[                    O   ����    e�          O   ����    R�          O   ����    ��            ��   *             �)               ��   4*              *               ��                  (*           ��                            ����                            submitValidation                                (+  +      ��                  �  �  @+              �c                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �+             X+               ��                  �+           ��                            ����                            synchronizeProperties                               �,  l,      ��                  �  �  �,              �                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �,             �,               ��                  �,           ��                            ����                            transferToExcel                             �-  �-      ��                  	    �-              �;                    O   ����    e�          O   ����    R�          O   ����    ��            ��   <.             .               ��   d.             0.               ��   �.             X.               ��                  �.           ��                            ����                            undoTransaction                             |/  d/      ��                      �/              Xi                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateAddQueryWhere                             �0  l0      ��                      �0              j                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �0             �0               ��                  �0           ��                            ����                            updateQueryPosition                             �1  �1      ��                      �1              hr                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateState                             �2  �2      ��                      �2               A                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  3           ��                            ����                            addRow          t3      �3     �       CHARACTER,INPUT pcViewColList CHARACTER cancelRow   |3      �3      �3   	 �       CHARACTER,  canNavigate �3      �3      (4    �       LOGICAL,    closeQuery  4      44      `4   
 �       LOGICAL,    columnProps @4      l4      �4    �       CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER  colValues   x4      �4      5   	 �       CHARACTER,INPUT pcViewColList CHARACTER copyRow �4      ,5      T5    �       CHARACTER,INPUT pcViewColList CHARACTER createRow   45      |5      �5   	 �       LOGICAL,INPUT pcValueList CHARACTER deleteRow   �5      �5      �5   	 �       LOGICAL,INPUT pcRowIdent CHARACTER  fetchRow    �5      6      H6  	  �       CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER fetchRowIdent   (6      �6      �6  
  �       CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER  findRow �6      �6       7          LOGICAL,INPUT pcKeyValues CHARACTER findRowWhere     7      D7      t7          LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  firstRowIds T7      �7      �7          CHARACTER,INPUT pcQueryString CHARACTER getLastCommitErrorType  �7       8      X8    )      CHARACTER,  hasForeignKeyChanged    88      d8      �8    @      LOGICAL,    openDataQuery   |8      �8      �8    U      LOGICAL,INPUT pcPosition CHARACTER  openQuery   �8      �8      (9   	 c      LOGICAL,    prepareQuery    9      49      d9    m      LOGICAL,INPUT pcQuery CHARACTER rowAvailable    D9      �9      �9    z      LOGICAL,INPUT pcDirection CHARACTER rowValues   �9      �9      :   	 �      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER    submitRow   �9      `:      �:   	 �      LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER  updateRow   l:      �:      �:   	 �      LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER getObjectType   �:      8;      h;    �      CHARACTER,  assignDBRow                             <  �;      ��                      <              \�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 4<  
         ��                            ����                            bufferCopyDBToRO                                4=  =      ��                      L=              Ę                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �=             d=  
             �� 
  �=             �=  
             ��   �=             �=               ��                  �=           ��                            ����                            compareDBRow                                �>  �>      ��                      �>              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmContinue                             �?  �?      ��                      �?              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  @           ��                            ����                            dataAvailable                               A  �@      ��                       A              x�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  8A           ��                            ����                            fetchDBRowForUpdate                             8B   B      ��                      PB              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchFirst                              8C   C      ��                      PC              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               8D   D      ��                       PD              l�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               8E   E      ��                  "  #  PE              t�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               8F   F      ��                  %  &  PF                                  O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            filterContainerHandler                              DG  ,G      ��                  (  *  \G              ȡ                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 tG  
         ��                            ����                            initializeObject                                tH  \H      ��                  ,  -  �H              $�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            refetchDBRow                                xI  `I      ��                  /  1  �I              <�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �I  
         ��                            ����                            releaseDBRow                                �J  �J      ��                  3  4  �J              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            startFilter                             �K  �K      ��                  6  7  �K              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            transferDBRow                               �L  �L      ��                  9  <  �L              4�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   M             �L               ��                   M           ��                            ����                            addQueryWhere   H;      hM      �M    �      LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER    assignQuerySelection    xM      �M      $N    �      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  bufferCompareDBToRO N      |N      �N    �      LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER bufferWhereClause   �N      O      PO    �      CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER  columnDataType  0O      �O      �O          CHARACTER,INPUT pcColumn CHARACTER  columnDbColumn  �O      �O      P          CHARACTER,INPUT pcColumn CHARACTER  columnQuerySelection    �O      4P      lP    -      CHARACTER,INPUT pcColumn CHARACTER  columnTable LP      �P      �P    B      CHARACTER,INPUT pcColumn CHARACTER  columnValMsg    �P      �P      Q     N      CHARACTER,INPUT pcColumn CHARACTER  dbColumnDataName    �P      4Q      hQ  !  [      CHARACTER,INPUT pcDbColumn CHARACTER    dbColumnHandle  HQ      �Q      �Q  "  l      HANDLE,INPUT pcColumn CHARACTER excludeColumns  �Q      �Q      R  #  {      CHARACTER,INPUT iTable INTEGER  getDataColumns  �Q      0R      `R  $  �      CHARACTER,  getForeignValues    @R      lR      �R  %  �      CHARACTER,  getQueryPosition    �R      �R      �R  &  �      CHARACTER,  getQuerySort    �R      �R      S  '  �      CHARACTER,  getQueryString  �R      (S      XS  (  �      CHARACTER,  getQueryWhere   8S      dS      �S  )  �      CHARACTER,  getTargetProcedure  tS      �S      �S  *  �      HANDLE, indexInformation    �S      �S      T  +  �      CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER   insertExpression    �S      lT      �T  ,  	      CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER  newQueryString  �T      �T      (U  -        CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER  newQueryValidate    U      �U      �U  .  )      CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER   newQueryWhere   �U      dV      �V  /  :      CHARACTER,INPUT pcWhere CHARACTER   newWhereClause  tV      �V      �V  0  H      CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER refreshRowident �V      XW      �W  1  W      CHARACTER,INPUT pcRowident CHARACTER    removeForeignKey    hW      �W      �W  2  g      LOGICAL,    removeQuerySelection    �W      �W      (X  3  x      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER   resolveBuffer   X      hX      �X  4  �      CHARACTER,INPUT pcBuffer CHARACTER  rowidWhere  xX      �X      �X  5 
 �      CHARACTER,INPUT pcWhere CHARACTER   rowidWhereCols  �X      Y      <Y  6  �      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER    setQueryPosition    Y      �Y      �Y  7  �      LOGICAL,INPUT pcPosition CHARACTER  setQuerySort    �Y      �Y       Z  8  �      LOGICAL,INPUT pcSort CHARACTER  setQueryString   Z      @Z      pZ  9  �      LOGICAL,INPUT pcQueryString CHARACTER   setQueryWhere   PZ      �Z      �Z  :  �      LOGICAL,INPUT pcWhere CHARACTER whereClauseBuffer   �Z      �Z      [  ;  �      CHARACTER,INPUT pcWhere CHARACTER   bindServer                              �[  �[      ��                  �  �  �[              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �\  �\      ��                  �  �  �\              (                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �]  �]      ��                  �  �  �]              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �^  �^      ��                  �  �  �^              <#                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �_  �_      ��                  �  �  `              �#                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �`  �`      ��                  �  �  a              L'                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �a  �a      ��                  �  �  b              �*                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 ,b  
         ��                            ����                            startServerObject                               ,c  c      ��                  �  �  Dc              `S                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                0d  d      ��                  �  �  Hd              �T                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  `d           ��                            ����                            getAppService   �Z      �d      �d  <        CHARACTER,  getASBound  �d      e      0e  = 
       LOGICAL,    getAsDivision   e      <e      le  >        CHARACTER,  getASHandle Le      xe      �e  ?  )      HANDLE, getASHasStarted �e      �e      �e  @  5      LOGICAL,    getASInfo   �e      �e      f  A 	 E      CHARACTER,  getASInitializeOnRun    �e       f      Xf  B  O      LOGICAL,    getASUsePrompt  8f      df      �f  C  d      LOGICAL,    getServerFileName   tf      �f      �f  D  s      CHARACTER,  getServerOperatingMode  �f      �f      g  E  �      CHARACTER,  runServerProcedure  �f      $g      Xg  F  �      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   8g      �g      �g  G  �      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   �g      �g      $h  H  �      LOGICAL,INPUT pcDivision CHARACTER  setASHandle h      Hh      th  I  �      LOGICAL,INPUT phASHandle HANDLE setASInfo   Th      �h      �h  J 	 �      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    �h      �h      i  K  �      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �h      <i      li  L  �      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   Li      �i      �i  M        LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  �i      �i      j  N        LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �j  �j      ��                  �  �  �j              dO�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  <k             k  
             ��   dk             0k               �� 
                 Xk  
         ��                            ����                            addMessage                              Pl  8l      ��                  �  �  hl              �n�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �l             �l               ��   �l             �l               ��                  �l           ��                            ����                            adjustTabOrder                              �m  �m      ��                  �  �  �m              �%�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  0n             �m  
             �� 
  Xn             $n  
             ��                  Ln           ��                            ����                            applyEntry                              Do  ,o      ��                  �  �  \o              H1�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  to           ��                            ����                            changeCursor                                pp  Xp      ��                  �  �  �p               �                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �p           ��                            ����                            createControls                              �q  �q      ��                  �  �  �q              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �r  �r      ��                  �  �  �r              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �s  �s      ��                  �  �  �s              t��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �t  �t      ��                  �  �  �t                �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �u  �u      ��                  �  �  �u              �ڐ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �v  �v      ��                  �  �  �v              hې                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �w  �w      ��                  �  �  �w              �o�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              �x  �x      ��                  �  �  �x              �p�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  $y             �x  
             ��   Ly             y               ��   ty             @y               ��                  hy           ��                            ����                            modifyUserLinks                             dz  Lz      ��                  �  �  |z              �h�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �z             �z               ��   �z             �z               �� 
                 �z  
         ��                            ����                            removeAllLinks                              �{  �{      ��                  �  �  �{              XL�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              �|  �|      ��                  �  �  �|              ،�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  D}             }  
             ��   l}             8}               �� 
                 `}  
         ��                            ����                            repositionObject                                `~  H~      ��                  �  �  x~              �s�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �~             �~               ��                  �~           ��                            ����                            returnFocus                             �  �      ��                  �     �              D!�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �  
         ��                            ����                            showMessageProcedure                                �  ̀      ��                      ��              �-�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   H�             �               ��                  <�           ��                            ����                            toggleData                              4�  �      ��                    	  L�              ,|�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  d�           ��                            ����                            viewObject                              \�  D�      ��                      t�              �|�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �i      ̃      ��  O 
 |      LOGICAL,    assignLinkProperty  ؃      �      8�  P  �      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   �      ��      ��  Q  �      CHARACTER,  getChildDataKey ��      ̄      ��  R  �      CHARACTER,  getContainerHandle  ܄      �      <�  S  �      HANDLE, getContainerHidden  �      D�      x�  T  �      LOGICAL,    getContainerSource  X�      ��      ��  U  �      HANDLE, getContainerSourceEvents    ��      ��      ��  V  �      CHARACTER,  getContainerType    ܅      �      <�  W  
      CHARACTER,  getDataLinksEnabled �      H�      |�  X        LOGICAL,    getDataSource   \�      ��      ��  Y  /      HANDLE, getDataSourceEvents ��      ��      �  Z  =      CHARACTER,  getDataSourceNames  Ԇ       �      4�  [  Q      CHARACTER,  getDataTarget   �      @�      p�  \  d      CHARACTER,  getDataTargetEvents P�      |�      ��  ]  r      CHARACTER,  getDBAware  ��      ��      �  ^ 
 �      LOGICAL,    getDesignDataObject ȇ      �      (�  _  �      CHARACTER,  getDynamicObject    �      4�      h�  `  �      LOGICAL,    getInstanceProperties   H�      t�      ��  a  �      CHARACTER,  getLogicalObjectName    ��      ��      ��  b  �      CHARACTER,  getLogicalVersion   Ј      ��      0�  c  �      CHARACTER,  getObjectHidden �      <�      l�  d  �      LOGICAL,    getObjectInitialized    L�      x�      ��  e        LOGICAL,    getObjectName   ��      ��      �  f        CHARACTER,  getObjectPage   ̉      ��      (�  g  &      INTEGER,    getObjectParent �      4�      d�  h  4      HANDLE, getObjectVersion    D�      l�      ��  i  D      CHARACTER,  getObjectVersionNumber  ��      ��      �  j  U      CHARACTER,  getParentDataKey    Ċ      ��      $�  k  l      CHARACTER,  getPassThroughLinks �      0�      d�  l  }      CHARACTER,  getPhysicalObjectName   D�      p�      ��  m  �      CHARACTER,  getPhysicalVersion  ��      ��      �  n  �      CHARACTER,  getPropertyDialog   ȋ      �      (�  o  �      CHARACTER,  getQueryObject  �      4�      d�  p  �      LOGICAL,    getRunAttribute D�      p�      ��  q  �      CHARACTER,  getSupportedLinks   ��      ��      ��  r  �      CHARACTER,  getTranslatableProperties   ��      �      (�  s  �      CHARACTER,  getUIBMode  �      4�      `�  t 
 	      CHARACTER,  getUserProperty @�      l�      ��  u  "	      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    |�      č      ��  v  2	      CHARACTER,INPUT pcPropList CHARACTER    linkHandles ܍      $�      P�  w  G	      CHARACTER,INPUT pcLink CHARACTER    linkProperty    0�      t�      ��  x  S	      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry ��      ��      �  y  `	      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   �      x�      ��  z  l	      CHARACTER,INPUT piMessage INTEGER   propertyType    ��      ̏      ��  {  z	      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  ܏      $�      T�  |  �	      CHARACTER,  setChildDataKey 4�      `�      ��  }  �	      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  p�      ��      �  ~  �	      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  ̐      �      @�    �	      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents     �      `�      ��  �  �	      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled |�      ��      ��  �  �	      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   ԑ      �      L�  �  �	      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents ,�      l�      ��  �  
      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  ��      Ȓ      ��  �  
      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   ܒ      $�      T�  �  .
      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents 4�      x�      ��  �  <
      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  ��      Г      ��  � 
 P
      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject ܓ      �      P�  �  [
      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    0�      x�      ��  �  o
      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   ��      Ȕ       �  �  �
      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    ��      $�      \�  �  �
      LOGICAL,INPUT c CHARACTER   setLogicalVersion   <�      x�      ��  �  �
      LOGICAL,INPUT cVersion CHARACTER    setObjectName   ��      Е       �  �  �
      LOGICAL,INPUT pcName CHARACTER  setObjectParent ��       �      P�  �  �
      LOGICAL,INPUT phParent HANDLE   setObjectVersion    0�      p�      ��  �  �
      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    ��      ̖       �  �  �
      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks ��      (�      \�  �  �
      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   <�      |�      ��  �        LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  ��      ԗ      �  �  '      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute �      ,�      \�  �  :      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   <�      ��      ��  �  J      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   ��      ܘ      �  �  \      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  ��      <�      h�  � 
 v      LOGICAL,INPUT pcMode CHARACTER  setUserProperty H�      ��      ��  �  �      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ��      ��      $�  �  �      LOGICAL,INPUT pcMessage CHARACTER   Signature   �      H�      t�  � 	 �      CHARACTER,INPUT pcName CHARACTER    l�    "  ��  0�      �       4   �����                 @�                      ��                  #  P                  ��                       #  Ě        $  \�  ؛      �       4   �����                 �                      ��                  %  O                  @�                       %  l�  �    <  �  ��      �       4   �����                 ��                      ��                  H  J                  ��                       H  �         I                                  ,     
                    � ߱        �  $  L  ��  ���                           $  N  @�  ���                       x                         � ߱        x�    T  ��  �      �      4   �����                �                      ��                  U  	                  x��                       U  ��  H�  o   X      ,                                 ��  $   Y  t�  ���                       �  @         �              � ߱        ��  �   Z        Ȟ  �   [  �      ܞ  �   ]        �  �   _  x      �  �   a  �      �  �   c  `      ,�  �   d  �      @�  �   e        T�  �   h  �      h�  �   j         |�  �   k  |      ��  �   m  �      ��  �   n  t      ��  �   o  �      ̟  �   p  ,      ��  �   q  �      ��  �   w  �      �  �   y  P	      �  �     �	      0�  �   �   
      D�  �   �  t
      X�  �   �  �
      l�  �   �  l      ��  �   �  �      ��  �   �  \      ��  �   �  �      ��  �   �  D      Р  �   �  �      �  �   �  �      ��  �   �  0      �  �   �  �       �  �   �  �      4�  �   �        H�  �   �  X      \�  �   �  �      p�  �   �        ��  �   �  L      ��  �   �  �      ��  �   �  �      ��  �   �         ԡ  �   �  <      �  �   �  x      ��  �   �  �      �  �   �  �          �   �  ,                      <�          ��  ��      ��                  @	  n	  ��              l��                    O   ����    e�          O   ����    R�          O   ����    ��      �     
                                     (                         � ߱        h�  $ T	  آ  ���                           O   l	  ��  ��  h               ԣ          ģ  ̣    ��                                             ��                            ����                                8;      $�      ��     V     ܣ                       أ  �                     8�    �	  ��  �      t      4   ����t                 �                      ��                  �	  
                  ,s�                       �	  ��  4�  �   �	  �      H�  �   �	  H      \�  �   �	  �      p�  �   �	  @      ��  �   �	  �      ��  �   �	  8      ��  �   �	  �      ��  �   �	  (      ԥ  �   �	  �      �  �   �	         ��  �   �	  �      �  �   �	        $�  �   �	  �          �   �	        ��    E
  T�  Ц      x      4   ����x                �                      ��                  F
  �
                  ���                       F
  d�  ��  �   H
  �      �  �   I
  T      �  �   J
  �      0�  �   K
  D      D�  �   L
  �      X�  �   M
  �      l�  �   O
  p      ��  �   P
  �      ��  �   Q
  X      ��  �   R
  �      ��  �   S
  �      Ч  �   T
  D       �  �   U
  �       ��  �   V
  �       �  �   W
  x!       �  �   X
  �!      4�  �   Y
  h"      H�  �   Z
  �"      \�  �   [
  `#      p�  �   \
  �#      ��  �   ]
  X$      ��  �   ^
  �$      ��  �   _
  �$      ��  �   `
  L%      Ԩ  �   a
  �%      �  �   b
  <&      ��  �   c
  �&      �  �   d
  4'      $�  �   e
  �'      8�  �   f
  ,(      L�  �   g
  h(      `�  �   i
  �(      t�  �   j
  X)      ��  �   k
  �)      ��  �   l
  *      ��  �   m
  �*      ĩ  �   n
  �*      ة  �   o
  l+      �  �   p
  �+       �  �   q
  \,      �  �   r
  �,      (�  �   s
  L-      <�  �   t
  �-      P�  �   u
  <.      d�  �   v
  �.      x�  �   w
  4/      ��  �   x
  �/          �   y
  $0      d�    �
  ��  8�      T0      4   ����T0                H�                      ��                  �
  �                  l�                       �
  ̪  \�  �   �
  �0      p�  �   �
  (1      ��  �   �
  �1      ��  �   �
  2      ��  �   �
  �2      ��  �   �
  3      ԫ  �   �
  |3      �  �   �
  �3      ��  �   �
  t4      �  �      �4      $�  �     l5      8�  �     �5      L�  �     d6      `�  �     �6      t�  �     L7      ��  �     �7      ��  �     <8      ��  �     �8      Ĭ  �   	  ,9      ج  �   
  �9      �  �     :       �  �     X:      �  �     �:      (�  �     H;      <�  �     �;      P�  �     8<          �     �<      x�    �  ��  ��      =      4   ����=  	              �                      ��             	     �  1                  P�                       �  ��   �  �   �  |=      4�  �   �  �=      H�  �   �  t>      \�  �   �  �>      p�  �   �  l?      ��  �   �  �?      ��  �   �  \@      ��  �   �  �@      ��  �   �  TA      Ԯ  �   �  �A      �  �   �  DB      ��  �   �  �B      �  �   �  <C      $�  �   �  �C      8�  �   �  ,D      L�  �   �  �D      `�  �   �  $E      t�  �   �  �E      ��  �   �  F      ��  �   �  �F      ��  �   �  G      į  �   �  �G      د  �   �  �G      �  �   �  8H       �  �   �  �H      �  �   �  0I      (�  �   �  �I      <�  �   �   J          �   �  �J      getRowObjUpdStatic  deleteRecordStatic  �    �  ��  ��      K      4   ����K      /   �  а     �                          3   ����K             �                      3   ����<K  Զ    �  ,�  ��  �  XK      4   ����XK  
              ��                      ��             
     �  S                  �J�                       �  <�  ̱  �   �  �K      $�  $  �  ��  ���                       �K     
                    � ߱        8�  �   �  L      ��  $   �  d�  ���                       ,L  @         L              � ߱        L�  $  �  ��  ���                       �L       	       	           � ߱        �M     
                N                     \O  @        
 O              � ߱        ܳ  V     �  ���                        hO       	       	       �O       
       
       �O       	       	           � ߱        l�  $  "  x�  ���                       �P     
                Q                     dR  @        
 $R              � ߱            V   4  �  ���                                      ̵                      ��                  U  �                  �P�                       U  ��  pR     
                �R                     <T  @        
 �S          �T  @        
 dT          U  @        
 �T          dU  @        
 $U              � ߱            V   j  �  ���                        adm-clone-props �  ��              �     W     `                          \  �                     start-super-proc    �  d�  �           �     X                                  �                     l�    
  �   �      �X      4   �����X      /     ,�     <�                          3   ���� Y            \�                      3   ���� Y  ķ  $  %  ��  ���                       @Y                         � ߱        ��    5  �  \�  ��  \Y      4   ����\Y                и                      ��                  6  :                  �?�                       6  �  pY                     �Y                     �Y                         � ߱            $  7  l�  ���                             ;  �  T�      �Y      4   �����Y  �Y                         � ߱            $  <  (�  ���                       |�    C  ��  ��  �  �Y      4   �����Y      $  D  ع  ���                       Z                         � ߱            �   a  Z      XZ     
                �Z                     $\  @        
 �[              � ߱        ��  V   u  �  ���                        ��  �   �  0\      ��    '  غ  �      p\      4   ����p\      /   (  �     $�                          3   �����\            D�                      3   �����\  �\     
                <]                     �^  @        
 L^              � ߱        H�  V   4  T�  ���                        �^     
                T_                     �`  @        
 d`              � ߱        t�  V   X  �  ���                        ��    �  ��  �      �`      4   �����`                �                      ��                  �  �                  t��                       �  ��  ��  /   �  H�     X�                          3   �����`            x�                      3   �����`      /   �  ��     Ľ                          3   ����a            �                      3   ����$a  p�  /  E   �         Xa                      3   ����@a  initProps   x�  0�              �     Y     \             �          X  �  	                                   t�          �  �      ��                �  �  4�              �                    O   ����    e�          O   ����    R�          O   ����    ��      �                      L�          H�  p   �  �}  ��      �  ��  �     �}                �                      ��                  �  �                  l|�                       �  ��  4�  :  �                 $  �  `�  ���                       �}                         � ߱        ��  �     �}                �                      ��                  �  �                  L��                       �  ��  0�  :  �                 $  �  \�  ���                       �}                         � ߱        �  �     ~                                        ��                  �                    ���                       �  ��  ��  ��     $~                                        ��                  	  %                  |}�                       	  $�  ,�  �     8~                                        ��                  &  B                  L~�                       &  ��  ��  ��     L~                                        ��                  C  _                  �                       C  <�  D�  4�     `~                                        ��                  `  |                  ��                       `  ��  ��  ��     t~                                        ��                  }  �                  d��                       }  T�  \�  L�     �~  	                                      ��             	     �  �                  ؉�                       �  ��  ��  ��     �~  
                                      ��             
     �  �                  ���                       �  l�  t�  d�     �~                                        ��                  �  �                  x��                       �  ��   �  ��     �~                                        ��                  �                    H��                       �  ��  ��  |�     �~                                        ��                    *                  �qL                         �  �  �     �~                                        ��                  +  G                  �rL                       +  ��  ��  ��                                              ��                  H  d                  �sL                       H  (�  0�   �                                             ��                  e  �                  TtL                       e  ��  ��  ��     (                                        ��                  �  �                  $uL                       �  @�      8�     <                                        ��                  �  �                  ��r                       �  ��      O   �  ��  ��  P               ��          ��  ��   , ��                                                       �     ��                            ����                            <�  d�  X�  ��      `�     Z     ��                      � ��  �                     ,�    �  ��  �      \      4   ����\                �                      ��                  �  �                  �r                       �  ��  ��  /   �  D�     T�                          3   ����l            t�                      3   �����  ��  /   �  ��     ��                          3   �����            ��                      3   �����  \�  /   �  �     ,�                          3   �����            L�                      3   ���� �      /   �  ��     ��                          3   ���� �            ��                      3   ����@�  `�     
                ܀                     ,�  @        
 �              � ߱        X�  V   E  ��  ���                        �  $  _  ��  ���                       @�                         � ߱        d�     
                ��                     0�  @        
 ��              � ߱        @�  V   i  ��  ���                        ��  $  �  l�  ���                       <�     
                    � ߱        P�     
                ̄                     �  @        
 ܅              � ߱        (�  V   �  ��  ���                        ��  $  �  T�  ���                       (�     
                    � ߱        <�     
                ��                     �  @        
 ȇ              � ߱        �  V   �  ��  ���                        ��  $  �  <�  ���                        �                         � ߱        H�     
                Ĉ                     �  @        
 ԉ              � ߱        ��  V   �  h�  ���                        �  �   �  ,�      ��  $  �  8�  ���                       L�     
                    � ߱        `�     
                ܊                     ,�  @        
 �              � ߱        ��  V   �  d�  ���                        L�  $     �  ���                       8�     
                    � ߱        `�  �   /  L�      ��  $  9  ��  ���                       ��     
                    � ߱        ��  �   S  ��      $�  $  u  ��  ���                       ��                         � ߱              �  @�  P�      ��      4   ������      /   �  |�     ��                          3   �����  ��     
   ��                      3   ����<�  ��        ��                      3   ����D�  �        �                      3   ����X�            <�                      3   ����t�  pushRowObjUpdTable  �  L�  �                   [      �                               g                      pushTableAndValidate    `�  ��  �           |     \     �                          �  �                      remoteCommit    ��  0�  �           p      ]     �                          �  �                      serverCommit    @�  ��  �           l    ! ^     �                          �  �                                      ��          ��  t�      ��                  �  �  ��              x�                    O   ����    e�          O   ����    R�          O   ����    ��          O   �  ��  ��  ��    ��                            ����                            ��  P�      �              _      ��                      
�     �                      disable_UI  �  p�                      `      �                               �   
                   initializeObject    |�  ��                      a      �                               !                      �  �    ����  �       ��         ��  8   ����   ��  8   ����   ��  8   ����   ��  8   ����   ��  8   ����   ��  8   ����       8   ����       8   ����        �  �      viewObject  ,   ��   �  ,�      toggleData  ,INPUT plEnabled LOGICAL    �  X�  p�      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  H�  ��  ��      returnFocus ,INPUT hTarget HANDLE   ��  ��  ��      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    ��  8�  D�      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE (�  ��  ��      removeAllLinks  ,   ��  ��  ��      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE ��  $�  8�      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    �  ��  ��      hideObject  ,   ��  ��  ��      exitObject  ,   ��  ��  �      editInstanceProperties  ,   ��  �  ,�      displayLinks    ,   �  @�  P�      createControls  ,   0�  d�  t�      changeCursor    ,INPUT pcCursor CHARACTER   T�  ��  ��      applyEntry  ,INPUT pcField CHARACTER    ��  ��  ��      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER ��  @�  L�      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER 0�  ��  ��      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��   �  �      unbindServer    ,INPUT pcMode CHARACTER ��  8�  H�      runServerObject ,INPUT phAppService HANDLE  (�  t�  ��      disconnectObject    ,   d�  ��  ��      destroyObject   ,   ��  ��  ��      bindServer  ,   ��  ��  ��      transferDBRow   ,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER  ��  4�  @�      startFilter ,   $�  T�  d�      releaseDBRow    ,   D�  x�  ��      refetchDBRow    ,INPUT phRowObjUpd HANDLE   h�  ��  ��      filterContainerHandler  ,INPUT phFilterContainer HANDLE ��  ��  �      fetchDBRowForUpdate ,   ��  $�  4�      confirmContinue ,INPUT-OUTPUT pioCancel LOGICAL �  d�  t�      compareDBRow    ,   T�  ��  ��      bufferCopyDBToRO    ,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER   x�  �  �      assignDBRow ,INPUT phRowObjUpd HANDLE    �  H�  T�      updateState ,INPUT pcState CHARACTER    8�  ��  ��      updateQueryPosition ,   p�  ��  ��      updateAddQueryWhere ,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER    ��   �  �      undoTransaction ,   ��  $�  4�      transferToExcel ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER  �  ��  ��      synchronizeProperties   ,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER   ��  ,�  @�      submitValidation    ,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER   �  ��  ��      submitForeignKey    ,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER  |�  �  $�      submitCommit    ,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL  �  h�  |�      startServerObject   ,   X�  ��  ��      setPropertyList ,INPUT pcProperties CHARACTER   ��  ��  ��      serverFetchRowObjUpdTable   ,OUTPUT TABLE-HANDLE phRowObjUpd    ��   �  0�      serverSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject    �  ��   �      saveContextAndDestroy   ,OUTPUT pcContext CHARACTER ��  ,�  <�      rowObjectState  ,INPUT pcState CHARACTER    �  h�  x�      retrieveFilter  ,   X�  ��  ��      restartServerObject ,   |�  ��  ��      remoteSendRows  ,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject,OUTPUT pocMessages CHARACTER   ��  ��  ��      refreshRow  ,   ��  ��  ��      printToCrystal  ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER  ��  P�  `�      isUpdatePending ,INPUT-OUTPUT plUpdate LOGICAL  @�  ��  ��      initializeServerObject  ,   ��  ��  ��      home    ,   ��  ��  ��      genContextList  ,OUTPUT pcContext CHARACTER ��  �   �      fetchPrev   ,   �  4�  @�      fetchNext   ,   $�  T�  `�      fetchLast   ,   D�  t�  ��      fetchFirst  ,   d�  ��  ��      fetchBatch  ,INPUT plForwards LOGICAL   ��  ��  ��      endClientDataRequest    ,   ��  ��  �      destroyServerObject ,   ��   �  0�      describeSchema  ,INPUT pcSdoName CHARACTER,OUTPUT TABLE-HANDLE hTtSchema    �  |�  ��      dataAvailable   ,INPUT pcRelative CHARACTER l�  ��  ��      copyColumns ,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE ��  �   �      commitTransaction   ,   ��  4�  D�      clientSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER    $�  ��  ��      batchServices   ,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER        � 
"     
 r%     adecomm/as-utils.w 
"   
   �    }        �
"     
    �     }        �� K   K   %               � 
"    
 %              h �P  \         (          
�                          
�            � �    
"    
 �
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        �    7%               
"   
 �               1� �  
 � �   %               o%   o           � �    
"   
 �           �    1� �   � �   %               o%   o           � �   
"   
 �           �    1� �  
 � �   %               o%   o           � �   
"   
 �           l    1� �   � �   %               o%   o           � �    
"   
 �           �    1�    � �   %               o%   o           �    
"   
 �           T    1� *   � 6   %               o%   o           %               
"   
 �          �    1� >   � N     
"   
 �               1� U   � �   %               o%   o           � h  
"   
 �           �    1� j   � �   %               o%   o           � y  S 
"   
 �           �    1� �   � 6   %               o%   o           %               
"   
 �           p    1� �   � 6   %               o%   o           %               
"   
 �           �    1� �   � 6   %               o%   o           %              
"   
 �          h    1� �   � 6     
"   
 �           �    1�   
 � 6   %               o%   o           %               
"   
 �                1�    � �   %               o%   o           � �    
"   
 �          �    1�    � N     
"   
 �           �    1� .   � �   %               o%   o           � D  t 
"   
 �          D	    1� �  
 � N     
"   
 �           �	    1� �   � �   %               o%   o           � �  � 
"   
 �           �	    1� b   � �   %               o%   o           � �    
"   
 �           h
    1� y  
 � �   %               o%   o           %               
"   
 ��           �
    1� �   �� 6   %               o%   o           %              
"   
 ��           `    1� �   �� �   %               o%   o           � �    �
"   
 ��           �    1� �   �� �   %               o%   o           o%   o           
"   
 ��           P    1� �  
 �� �   %               o%   o           � �    �
"   
 ��           �    1� �   �� �  	 %               o%   o           � �  / �
"   
 �          8    1�    � �  	   
"   
 ��           t    1�    �� �  	 o%   o           o%   o           � �    �
"   
 �          �    1� ,   � �  	   
"   
 ��           $    1� ;   �� �  	 o%   o           o%   o           � �    �
"   
 �          �    1� K   � 6     
"   
 �          �    1� Y   � �  	   
"   
 �              1� f   � �  	   
"   
 �          L    1� s   � �  	   
"   
 ��           �    1� �   �� 6   o%   o           o%   o           %              
"   
 �              1� �   � �  	   
"   
 �          @    1� �  
 � �     
"   
 �          |    1� �   � �  	   
"   
 �          �    1� �   � �  	   
"   
 �          �    1� �   � �  	   
"   
 �          0    1� �   � �  	   
"   
 �          l    1� �  	 � �  	   
"   
 �          �    1�    � �  	   
"   
 �          �    1�    � �  	   
"   
 ��                1� -   �� �   %               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 �
"   
   
"   
  (�  L ( l       �        �    �� 9   � P   �        �    �@    
� @  , 
�            �� B     p�               �L
�    %              � 8          � $         � I          
�    � c     
"   
 �� @  , 
�           �� �  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 ��           �    1� f  
 �� �   %               o%   o           � �    �
"   
 ��           <    1� q  
 �� �   %               o%   o           o%   o           
"   
 ��           �    1� |   �� N   %               o%   o           o%   o           
"   
 ��           4    1� �   �� 6   %               o%   o           %               
"   
 ��           �    1� �   �� 6   %               o%   o           %               
"   
 r�           ,    1� �   r� �   %               o%   o           � �    �
"   
 ��           �    1� �   �� 6   %               o%   o           %              
"   
 ��               1� �   �� 6   %               o%   o           o%   o           
"   
 ��           �    1� �   �� �   %               o%   o           o%   o           
"   
 ��               1� �  	 �� �   %               o%   o           � �    �
"   
 ��           �    1� �   �� �   %               o%   o           o%   o           
"   
 ��               1� �   �� �   %               o%   o           o%   o           
"   
 ��           �    1�    �� 6   %               o%   o           %               
"   
 ��           �    1�    �� 6   %               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 ��           �    1�   
 �� 6   %               o%   o           %              
"   
 ��           H    1� (   �� �   %               o%   o           o%   o           
"   
 ��           �    1� 4   �� �   %               o%   o           � �    �
"   
 ��           8    1� B   �� �   %               o%   o           o%   o           
"   
 �          �    1� N   � N     
"   
 ��           �    1� [   �� �   %               o%   o           � n  ! �
"   
 ��           d    1� �   �� �   %               o%   o           � �    �
"   
 ��           �    1� �   �� �   %               o%   o           � �   �
"   
 �          L    1� �   � �     
"   
 �          �    1� �   � N     
"   
 ��           �    1� �   �� �   %               o%   o           � �    �
"   
 �          8     1� �  
 � N     
"   
 r�           t     1� �   r� 6   %               o%   o           o%   o           
"   
 ��           �     1�    �� 6   %               o%   o           %               
"   
 ��           l!    1�    �� 6   %               o%   o           %               
"   
 ��           �!    1� )   �� �   %               o%   o           � �    �
"   
 ��           \"    1� 9   �� �   %               o%   o           o%   o           
"   
 ��           �"    1� E   �� 6   %               o%   o           %              
"   
 ��           T#    1� V   �� 6   %               o%   o           %               
"   
 r�           �#    1� c   r� 6   %               o%   o           %               
"   
 �          L$    1� s   � N     
"   
 �          �$    1� �   � �     
"   
 ��           �$    1� �   �� �   %               o%   o           o%   o           
"   
 ��           @%    1� �   �� �   %               o%   o           � �    �
"   
 ��           �%    1� �   �� �   %               o%   o           o%   o           
"   
 ��           0&    1� �   �� 6   o%   o           o%   o           o%   o           
"   
 ��           �&    1� �   �� �  	 %               o%   o           o%   o           
"   
 ��           ('    1� �   �� �   %               o%   o           o%   o           
"   
 ��           �'    1� �  
 �� �   %               o%   o           o%   o           
"   
 �           (    1� �   � �     
"   
 ��           \(    1�    �� �   %               o%   o           �   4 �
"   
 ��           �(    1� P  
 �� 6   %               o%   o           %              
"   
 �          L)    1� [   � N     
"   
 ��           �)    1� l   �� �   %               o%   o           � �    r
"   
 ��           �)    1� z   �� 6   %               o%   o           %              
"   
 ��           x*    1� �   �� �   %               o%   o           � �    �
"   
 ��           �*    1� �   �� �   %               o%   o           � �    �
"   
 ��           `+    1� �   �� �   %               o%   o           � �    �
"   
 ��           �+    1� �   �� 6   %               o%   o           %               
"   
 ��           P,    1� �  	 �� N   %               o%   o           o%   o           
"   
 r�           �,    1� �   r� �   %               o%   o           � �  	 �
"   
 ��           @-    1� �   �� �   %               o%   o           %       �       
"   
 ��           �-    1� �   �� �   %               o%   o           � �    �
"   
 ��           0.    1� �   �� 6   o%   o           o%   o           %              
"   
 ��           �.    1�    �� 6   %               o%   o           %               
"   
 ��           (/    1�    �� �   %               o%   o           o%   o           
"   
 ��           �/    1� /   �� �  	 %               o%   o           � �    �
"   
 �          0    1� @   � �  	   P �L 
�H T   %              �     }        �GG %              
"   
 r�           �0    1� M  
 r� �   %               o%   o           � �    r
"   
 ��           1    1� X   �� 6   %               o%   o           %               
"   
 ��           �1    1� e  	 �� �   %               o%   o           � �    �
"   
 ��           2    1� o   �� �   %               o%   o           � �    �
"   
 ��           �2    1� }   �� 6   %               o%   o           %               
"   
 ��           �2    1� �   �� �   %               o%   o           � �    �
"   
 ��           p3    1� �   �� �   %               o%   o           o%   o           
"   
 ��           �3    1� �   �� �   %               o%   o           o%   o           
"   
 ��           h4    1� �   �� 6   %               o%   o           o%   o           
"   
 r�           �4    1� �   r� 6   %               o%   o           o%   o           
"   
 ��           `5    1� �   �� 6   %               o%   o           o%   o           
"   
 ��           �5    1� �   �� �   %               o%   o           o%   o           
"   
 ��           X6    1� �  	 �� �  	 %               o%   o           � �    �
"   
 ��           �6    1� �  
 �� �  	 %               o%   o           � �    �
"   
 ��           @7    1�    �� �   %               o%   o           � �    �
"   
 ��           �7    1�    �� �   %               o%   o           o%   o           
"   
 ��           08    1� %   �� �   %               o%   o           o%   o           
"   
 ��           �8    1� 2   �� �   %               o%   o           � �    �
"   
 ��            9    1� G   �� �   %               o%   o           � �    �
"   
 ��           �9    1� V   �� �  	 %               o%   o           o%   o           
"   
 �          :    1� h   � N     
"   
 ��           L:    1� t   �� �   %               o%   o           � �    �
"   
 ��           �:    1� �   �� �   %               o%   o           o%   o           
"   
 r�           <;    1� �   r� 6   %               o%   o           o%   o           
"   
 ��           �;    1� �  
 �� �   %               o%   o           � �    �
"   
 ��           ,<    1� �   �� �   %               o%   o           � �    �
"   
 ��           �<    1� �   �� 6   %               o%   o           %               P �L 
�H T   %              �     }        �GG %              
"   
 ��           p=    1� �  	 �� N   %               o%   o           o%   o           
"   
 ��           �=    1� �   �� N   %               o%   o           o%   o           
"   
 ��           h>    1� �   �� N   %               o%   o           o%   o           
"   
 r�           �>    1�    r� 6   %               o%   o           %              
"   
 ��           `?    1�    �� �   %               o%   o           � 0  M r
"   
 ��           �?    1� ~   �� 6   %               o%   o           %              
"   
 ��           P@    1� �   �� 6   %               o%   o           %               
"   
 ��           �@    1� �   �� 6   %               o%   o           %               
"   
 ��           HA    1� �   �� �  	 %               o%   o           � �   �
"   
 ��           �A    1� �   �� 6   %               o%   o           %               
"   
 ��           8B    1� �   �� �  	 %               o%   o           o%   o           
"   
 ��           �B    1� �   �� 6   o%   o           o%   o           %              
"   
 r�           0C    1� 
   r� �  	 o%   o           o%   o           � �    r
"   
 ��           �C    1�    �� N   o%   o           o%   o           o%   o           
"   
 ��            D    1� -   �� N   o%   o           o%   o           o%   o           
"   
 ��           �D    1� =   �� �  	 o%   o           o%   o           o%   o           
"   
 ��           E    1� M   �� N   o%   o           o%   o           o%   o           
"   
 ��           �E    1� \   �� �  	 o%   o           o%   o           � j   �
"   
 ��           F    1� l   �� �  	 o%   o           o%   o           � {   �
"   
 ��           |F    1� �   �� 6   %               o%   o           %               
"   
 ��           �F    1� �   �� 6   %               o%   o           %               
"   
 �          tG    1� �   � �  	   
"   
 ��           �G    1� �   �� 6   %               o%   o           %               
"   
 ��           ,H    1� �   �� �   %               o%   o           o%   o           
"   
 ��           �H    1� �   �� �   %               o%   o           o%   o           
"   
 ��           $I    1� �   �� 6   %               o%   o           o%   o           
"   
 ��           �I    1� 	   �� �   %               o%   o           � �    �
"   
 ��           J    1�    �� &   %               o%   o           %               
"   
 ��           �J    1� .  	 �� 6   %               o%   o           %                "    %     start-super-proc A%     adm2/smart.p � P �L 
�H T   %              �     }        �GG %              
"   
   �       �K    6� 9     
"   
   
�        �K    8
"   
   �        �K    ��     }        �G 4              
"   
 ߱G %              G %              %�   AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets  
�H T   %              �     }        �GG %              
"   
  
"   
 
"   
  
"   
   (�  L ( l       �        �M    �� 9   � P   �        �M    �@    
� @  , 
�       �M    �� B    p�               �L
�    %              � 8       N    � $         � I          
�    � c    
"   
 �p� @  , 
�       O    �� U   �p�               �L"  	  , �   � d   �� f   �     }        �A      |    "  	    � d   �%              (<   \ (    |    �     }        �A� h   �A"  
  �    "  	   "  
  �  < "  	   "  
  �(    |    �     }        �A� h   �A"  
  �
�H T   %              �     }        �GG %              
"   
  
"   
 
"   
  
"   
   (�  L ( l       �        �P    �� 9   � P   �        �P    �@    
� @  , 
�       �P    �� B    p�               �L
�    %              � 8      Q    � $         � I          
�    � c    
"   
 �p� @  , 
�       R    �� �  
 �p�               �L"  	  , 
�H T   %              �     }        �GG %              
"   
   
"   
 �
"   
   
"   
   (�  L ( l       �        �R    �� 9   � P   �        �R    �@    
� @  , 
�       �R    �� B     p�               �L
�    %              � 8      �R    � $         � I          
�    � c     
"   
 �p� @  , 
�       �S    �� �  
 �p�               �L%     SmartDataObject 
"   
   p� @  , 
�       XT    �� �     p�               �L%               
"   
  p� @  , 
�       �T    �� ;    p�               �L%               
"   
  p� @  , 
�       U    ��     p�               �L(        � �      � �      � �      �     }        �A
�H T   %              �     }        �GG %              
"   
 � (   � 
"   
      �        �U    �� 9   �
"   
   � 8      DV    � $         � I          
�    � c    
"   
   �        �V    �
"   
   �       �V    /
"   
   
"   
   �       �V    6� 9     
"   
   
�        W    8
"   
   �        4W    �
"   
   �       TW    �
"   
   p�    � �   �
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
      �        X    �A"    �A
"   
   
�        dX    �@ � 
"   
 �"      �       }        �
"   
 %              %                "    %     start-super-proc A%     adm2/appserver.p ���    �      
�    �     }        �%               %      Server  - �     }        �    "    �� �    %                   "    �� �    %      NONE    p�,  8         $     "    �        � ,    
�    
�H T   %              �     }        �GG %              
"   
  
"   
 
"   
  
"   
   (�  L ( l       �        �Z    �� 9   � P   �        �Z    �@    
� @  , 
�       �Z    �� B    p�               �L
�    %              � 8      �Z    � $         � I          
�    � c    
"   
 �p� @  , 
�       �[    �� �   �p�               �L"    , p�,  8         $     "    �        � :    
�     "    %     start-super-proc @%     adm2/dataquery.p �r
�H T   %              �     }        �GG %              
"   
  
"   
 
"   
  
"   
  (�  L ( l       �        ]    �� 9   � P   �        ]    �@    
� @  , 
�       $]    �� B    p�               �L
�    %              � 8      0]    � $         � I         
�    � c    
"   
 �p� @  , 
�       @^    �� �   �p�               �L%H > 8   dataAvailable,confirmContinue,isUpdatePending,buildDataRequest  
�H T   %              �     }        �GG %              
"   
  
"   
 
"   
  
"   
  (�  L ( l       �        $_    �� 9   � P   �        0_    �@    
� @  , 
�       <_    �� B    p�               �L
�    %              � 8      H_    � $         � I         
�    � c    
"   
 �p� @  , 
�       X`    �� *   �p�               �L%               "    %     start-super-proc ?%     adm2/query.p � %     start-super-proc ?%     adm2/queryext.p % 	    initProps  
�    %L C <   FOR EACH Almmmatg NO-LOCK,       FIRST Almmmate OF Almmmatg NO-LOCK �   � �     � �     � �     
�     	         �G
"   
 ��        �a    �G
"   
   
"   
    x    (0 4      �        b    �G%                   �        b    �GG %              � �     � �         %              %                   "      %              
"   
       "      �        c    �
"   
   �        @c    �
"   
   
�       `c    �"       \      H   "     ((       "      %              � �      � �         
"   
   
"   
  \      H   "      ((       "      %              � �     � �   ��        d    �%                   %              %                   "  (    %                  "  (        
"   
  
"   
 �0 T       m � "  (  r�        e    �A @   "      $         � "  (  �� h   �        e    �� "  (    
"   
  \ H     H   "      ((       "     %              � �    � �     (        "  !   � �    ��        �e    �"  !  �
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
   
"   
   (�  L ( l       �        �f    �� 9   � P   �        �f    �@    
� @  , 
�       �f    �� B     p�               �L
�    %              � 8      �f    � $         � I          
�    � c     
"   
 �p� @  , 
�       �g    �� �   �p�               �L%               
"   
   p� @  , 
�       \h    �� 4     p�               �L"    , �,  8         $     "    �L        � �  
  
�    
�H T   %              �     }        �GG %              
"   
 $ 
"   
   
"   
  
"   
  (�  L ( l       �        @i    �� 9   � P   �        Li    �@    
� @  , 
�       Xi    �� B    p�               �L
�    %              � 8      di    � $         � I         
�    � c     
"   
 �p� @  , 
�       tj    �� h   �p�               �L
"   
 , 
"   
   p� @  , 
�       �j    �� G     p�               �L"    , 
"   
  p� @  , 
�       $k    �� �    p�               �L"    ,     "    �� �    %d Z T   OPEN QUERY Query-Main FOR EACH Almmmatg NO-LOCK,       FIRST Almmmate OF Almmmatg NO-LOCK.     "    mm� %   K,((        "    OF%                   "    � +     "      (   "           "    %              @ �,  8         $     "             � 7    
�    p�,  8         $     � D   �        � F    
�    %T J D   rowObject.CodCia-2 = Almmmatg.CodCia  rowObject.codmat-2 = Almmmatg.codmat  �    "      � �         %              %                   "      %                  "      "      "     T(        "    �%              "    �� �   "      �       "     �    "    �� h   � �      � h    �    "     � h    S    "      "        "    �%                � @    �     t T     P   4       "      (0       4       �"      � �      � �     � �   �T ,  %              T   "    �"    � �     � h    � �   �T    �    "    �� h   "      � h    "      %                   %              %                   "      %                  "      �     "      �     "       \      H   "      ((       "     %              � �    � �     4  �     "      
�H T   %              �     }        �GG %              
"   
  
"   
 
"   
  
"   
   (�  L ( l       �        �q    �� 9   � P   �         r    �@    
� @  , 
�       r    �� B    p�               �L
�    %              � 8      r    � $         � I          
�    � c    
"   
 �p� @  , 
�       (s    �� M  
 �p�               �L"    ,       "  
  ��    � �    �� �         "  	    �    � �  W � �   ��   � �     � �     � �     �   � �     � �    � �  W �      "  
  ��    � �    �� �         "  	    �    � �  ) � �   �   ,        "     � �   ��   � �    � �   �� �       ,        "      � �     �   � �   �� �   � �  ) ��   � �     � �     � !  �   
�H T   %              �     }        �GG %              
"   
 
"   
  
"   
 
"   
 (�  L ( l       �        �u    �� 9   � P   �        �u    �@    
� @  , 
�       �u    �� B   p�               �L
�    %              � 8      �u    � $         � I          
�    � c     
"   
 �p� @  , 
�       �v    �� �   �p�               �L"    , 
"   
   p� @  , 
�       Tw    �� �     p�               �L"    , 
"   
  p� @  , 
�       �w    �� �    p�               �L"    ,     %              %                   "      %                  "      �     "      �     "      4 (        "  
    �    � �      � �         "  	  ��     "    �T    "      "      @ A,    �   � �   � �     "     "       T      @   "    (        "      � �     � �      � �    "    �     "  	   %              D H   @ A,    �   � �    � �     "     "    �,    S   "     � �    �� �   %                T      @   "    (        "      � �     � �      � �    "    �     "  
   %                         "    � �     "                "      � �    "      
�H T   %              �     }        �GG %              
"   
 �
"   
   
"   
 �
"   
  (�  L ( l       �        �{    �� 9   � P   �        �{    �@    
� @  , 
�       �{    �� B   �p�               �L
�    %              � 8      �{    � $         � I         
�    � c   
"   
 �p� @  , 
�       �|    �� �   �p�               �L"    , 
"   
   p� @  , 
�       T}    �� �     p�               �L"    , "      %               �     }        �%               �     }        �%              %              %              %              %              %              %       	       %       
       %              %              %              %              %              %              %              %              "       "    %     start-super-proc >%     adm2/data.p %     start-super-proc >%     adm2/dataext.p %     start-super-proc >%     adm2/dataextcols.p %     start-super-proc >%     adm2/dataextapi.p �
�H T   %              �     }        �GG %              
"   
  
"   
 
"   
  
"   
  (�  L ( l       �        ��    �� 9   � P   �        ��    �@    
� @  , 
�       Ā    �� B    p�               �L
�    %              � 8      Ѐ    � $         � I         
�    � c    
"   
 �p� @  , 
�       ��    �� �   �p�               �L%               %      "alm/dmate-matg-p2.i" 
�H T   %              �     }        �GG %              
"   
  
"   
 
"   
  
"   
   (�  L ( l       �        ��    �� 9   � P   �        ��    �@    
� @  , 
�       Ȃ    �� B    p�               �L
�    %              � 8      Ԃ    � $         � I          
�    � c    
"   
 �p� @  , 
�       �    �� �   �p�               �L"    , 
�     	        �G
�H T   %              �     }        �GG %              
"   
  
"   
 
"   
  
"   
   (�  L ( l       �        ��    �� 9   � P   �        ��    �@    
� @  , 
�       ��    �� B    p�               �L
�    %              � 8      ��    � $         � I          
�    � c    
"   
 �p� @  , 
�       Ѕ    �� �  
 �p�               �L
"   
 , 
�     
        �G
�H T   %              �     }        �GG %              
"   
  
"   
 
"   
  
"   
   (�  L ( l       �        ��    �� 9   � P   �        ��    �@    
� @  , 
�       ��    �� B    p�               �L
�    %              � 8      ��    � $         � I          
�    � c    
"   
 �p� @  , 
�       ��    �� �  	 �p�               �L
"   
 , 
"   
      �    	   �        �    �
�H T   %              �     }        �GG %              
"   
  
"   
 
"   
  
"   
   (�  L ( l       �        ��    �� 9   � P   �        ��    �@    
� @  , 
�       ��    �� B    p�               �L
�    %              � 8      ��    � $         � I          
�    � c    
"   
 �p� @  , 
�       ȉ    �� )   �p�               �L"    , 
"   
   �        �    �"      
�     
        �G
�H T   %              �     }        �GG %              
"   
  
"   
 
"   
  
"   
   (�  L ( l       �        ��    �� 9   � P   �        ��    �@    
� @  , 
�       Ċ    �� B    p�               �L
�    %              � 8      Њ    � $         � I          
�    � c    
"   
 �p� @  , 
�       ��    �� �  	 �p�               �L
"   
 , 
�             �Gp�,  8         $     
"   
 �        �      
�    
�             �Gp�,  8         $     
"   
 �        � ,     
�    �    � >      
�        "    �� �    %     modifyListProperty 
�    %      REMOVE  %     SupportedLinks %     Update-Target  %     bufferValidate  
�    "      �  %      setContextAndInitialize 
�    "       %     bufferCommit    
�    "       "       �    � �      
�    %               %     bufferCommit    
�    " !     " !     
�     
        �G�     }        �
�    %      SUPER                   �           �   l       ��                 b  �  �               �R�                    O   ����    e�          O   ����    R�          O   ����    ��        $  q  �   ���                       �U     
                    � ߱              r  (  �      V      4   ����V                �                      ��                  s  �                  ��                       s  8  �  �  t  PV            v  �  `      �V      4   �����V                p                      ��                  w  �                  ,��                       w  �  �  o   x      ,                                 �  �   y  �V      �  �   z  �V      $  $  {  �  ���                        W     
                    � ߱        8  �   |  @W      L  �   }  `W      `  �   �  �W          $   �  �  ���                       �W  @         �W              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �  �  �               ���                    O   ����    e�          O   ����    R�          O   ����    ��      �                      �          �  $  �    ���                       X     
                    � ߱                  �  �                      ��                   �  �                  ���                     �  4      4   ����$X      $  �  �  ���                       pX     
                    � ߱        �    �  4  D      �X      4   �����X      /  �  p                               3   �����X  �  �   �  �X          O   �  ��  ��  �X                               , �                          
                               �      ��                            ����                                            �           �   l       ��            	     Q  �  �               ��                    O   ����    e�          O   ����    R�          O   ����    ��        $  q  �   ���                       `a                         � ߱        �  $  r  8  ���                       �a                         � ߱        �a     
                (b  @         �a              � ߱        D  $   �  d  ���                         T      �  x                      ��        0         �  �                  ��      Lc     L     �  �      $  �  �  ���                       �b                         � ߱          $  �  �  ���                       �b                         � ߱            4   ����c  lc     
                �c                     d                         � ߱          $  �    ���                                                            ��                  �  �                  �                �     �  �  �  $  �  L  ���                       �d       !       !           � ߱          �      L  �                      ��        0         �  �                  40�     ( e            �  x      $  �     ���                       �d       (       (           � ߱        �  $  �  x  ���                       �d       (       (           � ߱            4   �����d        �  �  L      (e      4   ����(e                \                      ��                  �  �                  \D�                       �  �  �  $  �  �  ���                       �e       !       !           � ߱            O   �  �� ��          $  �  �  ���                       �e                         � ߱        |f     
                �f                     Hh  @        
 h          �h  @        
 hh          �h                     �h     
                pi                     �j  @        
 �j          k  @        
 �j          pk  @        
 0k              � ߱        x  V   �  $  ���                        P	    �  �  $	      |k      4   ����|k  �k                     l                     $l                     �l                         � ߱            $  �  �  ���                       �	    �  l	  |	      �l      4   �����l      �   �   m      �	  $  �  �	  ���                       @m                         � ߱        �
  $  �  
  ���                       �m                         � ߱          �
                              ��        0         �  �                  ���      0n     �     �  @
      $  �  �
  ���                       �m                         � ߱        l  $  �  @  ���                       �m                         � ߱            4   ����n  <n                     �n                     �n                     �n                      o                         � ߱        D  $  �  |  ���                             �  `  p       o      4   ���� o      $  �  �  ���                       Ho          tp             � ߱        �  $  �  �  ���                       �p                         � ߱          �      �  \                      ��        0         �  �                  ���      q          �         $  �  �  ���                       �p                         � ߱        L  $  �     ���                       �p                         � ߱            4   �����p      $  �  �  ���                       (q                         � ߱        �q     
                $r                     ts  @        
 4s              � ߱        �  V   �  �  ���                        �s       
       
       �s       	       	       �s                     t                         � ߱        p  $    D  ���                       @t       
       
       tt       	       	       �t                     �t                         � ߱        �  $  A  �  ���                       �  $  �  �  ���                       Pu                         � ߱        |u     
                �u                     Hw  @        
 w          �w  @        
 `w          �w  @        
 �w              � ߱        (  V   �  �  ���                          8      �                        ��        0    	     .  C                        �x     �     .  �      $  .  d  ���                       x                         � ߱        �  $  .  �  ���                       4x                         � ߱        �  4   ����\x      4   �����x  `  $  3  4  ���                       �x                         � ߱        |    5  |  �      y      4   ����y                P                      ��                  6  :                  �                       6  �  `y                     �y       	       	           � ߱            $  7    ���                             <  �        �y      4   �����y  	              l                      ��             	     >  B                  ,                       >  �  �z                     �z       
       
           � ߱            $  ?  $  ���                       {                     H{                         � ߱        �  $  I  �  ���                       |{     
                �{                     H}  @        
 }          �}  @        
 `}              � ߱            V   W    ���                                    J           |  D  � X�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
             
                                                                                                                                                                                                                               ) �   �   �   �       (  8  H  X  h  x  �  �  �  �  �  �      (  8  H  X  h  x  �  �  �  �  �  �  �  �      (  8  H  8HXhx��������(8HX     ) �   �   �   �      (  8  H  X  h  x  �  �  �  �  �   �     (  8  H  X  h  x  �  �  �  �  �  �  �  �      (  8  H  8HXhx��������(8HX  �   :                  � �                     �    ��                      ��                            ����                            �                          ��                                �   l       ��                      �               ti                    O   ����    e�          O   ����    R�          O   ����    ��              !                    ��                            ����                                            �           �   l       ��                  (  7  �               Ll                    O   ����    e�          O   ����    R�          O   ����    ��      z        �              �                  $                  d  /  4  $     4  ��                      3   ������            T                      3   ������      O   5  ��  ��  ��               �          �  �    �                                             ��                            ����                                            H          �   l       ��                  A  l  �               tu                    O   ����    e�          O   ����    R�          O   ����    ��      �         �              �                $                  �         ,             �          �                                  �  /  `  t     �  �                      3   ����č            �                      3   ������     /  b  �     �  �                      3   ������  x                             3   ���� �      $   b  L  ���                                                    � ߱                  �  �                  3   ����,�      $   b  �  ���                                                    � ߱        X  $  f  ,  ���                       8�                          � ߱            O   j  ��  ��  T�                �          �  �   @ �                                                              0              0            ��                            ����                                                       �   l       ��                  v  �  �               |y                    O   ����    e�          O   ����    R�          O   ����    ��      �       $                  �    !                 �          �    !                   �              /  �  L     \  ��                      3   ����h�  �        |  �                  3   ������      $   �  �  ���                                !                   � ߱                                      3   ������      $   �  @  ���                                !                   � ߱                   !  �          �  �   , �                                                            !     ��                            ����                                            �           �   l       ��                      �               �                    O   ����    e�          O   ����    R�          O   ����    ��              �   �       ��      4   ������      �     ̎    ��                            ����                                            �           �   l       ��                    9  �               �                    O   ����    e�          O   ����    R�          O   ����    ��          /   5  �                                 3   ����Ԏ    ��                            ����                            TXS appSrvUtils s-codcia s-codalm Almmmatg Cat�logo de Materiales Almmmate D:\newsie\on_in_co\aplic\alm\dmate-matg-p2.w should only be RUN PERSISTENT. ADDROW CANCELROW CANNAVIGATE CLOSEQUERY COLUMNPROPS COLVALUES COPYROW CREATEROW DELETEROW FETCHROW FETCHROWIDENT FINDROW FINDROWWHERE FIRSTROWIDS GETLASTCOMMITERRORTYPE HASFOREIGNKEYCHANGED OPENDATAQUERY OPENQUERY PREPAREQUERY ROWAVAILABLE ROWVALUES SUBMITROW UPDATEROW GETOBJECTTYPE xiRocketIndexLimit ADDQUERYWHERE ASSIGNQUERYSELECTION BUFFERCOMPAREDBTORO BUFFERWHERECLAUSE COLUMNDATATYPE COLUMNDBCOLUMN COLUMNQUERYSELECTION COLUMNTABLE COLUMNVALMSG DBCOLUMNDATANAME DBCOLUMNHANDLE EXCLUDECOLUMNS GETDATACOLUMNS GETFOREIGNVALUES GETQUERYPOSITION GETQUERYSORT GETQUERYSTRING GETQUERYWHERE GETTARGETPROCEDURE INDEXINFORMATION INSERTEXPRESSION NEWQUERYSTRING NEWQUERYVALIDATE NEWQUERYWHERE NEWWHERECLAUSE REFRESHROWIDENT REMOVEFOREIGNKEY REMOVEQUERYSELECTION RESOLVEBUFFER ROWIDWHERE ROWIDWHERECOLS SETQUERYPOSITION SETQUERYSORT SETQUERYSTRING SETQUERYWHERE WHERECLAUSEBUFFER GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDataObject ContainerType PropertyDialog adm2/support/datad.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets SupportedLinks Data-Source,Data-Target,Navigation-Target,Update-Target,Commit-Target,Filter-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext AutoCommit BLOBColumns BufferHandles CLOBColumns CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState CurrentRowid ROWID CurrentUpdateSource DataColumns DataHandle DataIsFetched DataModified DataQueryBrowsed DataQueryString FetchOnOpen FillBatchOnRepos FilterActive FilterAvailable FilterSource FilterWindow FirstRowNum ForeignFields ForeignValues IgnoreTreeViewFilter IndexInformation LargeColumns LastRowNum NavigationSource NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter OpenOnInit PrimarySDOSource PromptColumns PromptOnDelete QueryColumns QueryPosition QueryString RebuildOnRepos RowObject RowObjectState NoUpdates RowsToBatch Tables ToggleDataTargets TransferChildrenForAll UpdatableColumns UpdatableWhenNew UpdateSource AssignList AuditEnabled BaseQuery CalcFieldList CheckLastOnOpen DataColumnsByTable DBNames EntityFields FetchHasAudit FetchHasComment FetchAutoComment FirstResultRow KeyFields KeyTableId LastDBRowIdent LastResultRow NewBatchInfo NoLockReadOnlyTables PhysicalTables PositionForClient QueryHandle QueryRowIdent RequiredProperties SkipTransferDBRow TempTables UpdatableColumnsByTable UpdateFromSource RowObjUpd RowObjectTable RowObjUpdTable CheckCurrentChanged StatelessSavedProperties CheckCurrentChanged,RowObjectState,LastResultRow,FirstResultRow,QueryRowIdent DestroyStateless DisconnectAppServer ServerSubmitValidation DataFieldDefs "alm/dmate-matg-p2.i" QueryContainer QueryContext AsynchronousSDO DataLogicProcedure DataLogicObject DataReadHandler DataReadColumns DataReadBuffer DataDelimiter | DataReadFormat TrimNumeric IsRowObjectExternal IsRowObjUpdExternal ManualAddQueryWhere DynamicData LastCommitErrorType LastCommitErrorKeys RunDataLogicProxy SchemaLocation CacheDuration INTEGER ShareData ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService adm2/dataquery.p dataAvailable,confirmContinue,isUpdatePending,buildDataRequest adm2/query.p adm2/queryext.p cTable iTable cColumns cDataCols cUpdCols cCalcData cCalcUpd iNumData iNumUpd cBuffers cKeyFields cAssignList iAssigns iPos iEntry iCount cTables cTableAssign cDbEntry cField cKeyTable cQueryString FOR EACH Almmmatg NO-LOCK,       FIRST Almmmate OF Almmmatg NO-LOCK ,   Almmmatg Almmmate hQuery cOpenQuery cDBNames cPhysicalTables cKeyTableEntityFields cKeyTableEntityValues cKeyTableEntityMnemonic cKeyTableEntityObjField cDBName cEntityFields lHasObjectField lHasAudit lHasComment lHasAutoComment iLookup iAlias  STATIC setDBNames OPEN QUERY Query-Main FOR EACH Almmmatg NO-LOCK,       FIRST Almmmate OF Almmmatg NO-LOCK.  FOR   PRESELECT  setOpenQuery 5 showMessage rowObject.CodCia-2 = Almmmatg.CodCia  rowObject.codmat-2 = Almmmatg.codmat ; CodCia-2 codfam CodMar codmat-2 CodPr1 DesMar DesMat TpoArt UndStk subfam FchIng Pesmat AlmDes CodAlm CodCia codmat CodUbi StkAct AlmDes CodAlm CodCia codmat CodUbi StkAct CodCia-2 codfam CodMar codmat-2 CodPr1 DesMar DesMat TpoArt UndStk subfam FchIng Pesmat Query-Main INITPROPS piTableIndex lRet DELETERECORDSTATIC adm2/data.p adm2/dataext.p adm2/dataextcols.p adm2/dataextapi.p AlmDes CodAlm CodCia codmat CodUbi StkAct CodCia-2 codfam CodMar codmat-2 CodPr1 DesMar DesMat TpoArt UndStk subfam FchIng Pesmat RowNum RowIdent RowMod RowIdentIdx RowUserProp ChangedFields cContainerType hRowObject hDataQuery cDataFieldDefs FOR EACH  setRowObjectTable setRowObjUpdTable getUpdatableColumns REMOVE Update-Target PUSHROWOBJUPDTABLE pcValType PUSHTABLEANDVALIDATE pcContext pcMessages pcUndoIds obtainContextForClient REMOTECOMMIT SERVERCOMMIT GETROWOBJUPDSTATIC DISABLE_UI INITIALIZEOBJECT qDataQuery h  3  �  dA      / �<   ��      0         pcServices      ��      T         pcServices  �   ��      x         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow      ��              piStartRow  <  ��      ,        pcViewColList       ��      T       
 pcViewColList       ��      |        pcRelative  �  ��      �        pcSdoName       ��      �  �     
 pcSdoName       ��      �        plForwards      ��              pcContext       ��      0        plUpdate    `  ��      T        pcFieldList �  ��      x        pcFieldList     ��      �        pcFieldList �  ��      �        piocContext �  ��      �        piocContext   ��              piocContext 8  ��      ,        piocContext \  ��      P        piocContext �  ��      t        piocContext �  ��      �  �     
 piocContext     ��      �        piocContext     ��      �        pcState     ��               pcContext   0  ��      $        piStartRow  T  ��      H        piStartRow  x  ��      l        piStartRow  �  ��      �        piStartRow  �  ��      �        piStartRow      ��      �  �     
 piStartRow      ��      �  �     
 phRowObjUpd     ��               pcProperties    T  ��      H        piStartRow  x  ��      l        piStartRow  �  ��      �        piStartRow  �  ��      �        piStartRow  �  ��      �        piStartRow      ��      �  �     
 piStartRow  ,  ��               pcRowIdent      ��      D        pcRowIdent  t  ��      h        pcRowIdent  �  ��      �        pcRowIdent      ��      �        pcRowIdent  �  ��      �        pcValueList     ��      �        pcValueList 4  ��              pcPropertiesForServer       ��      L        pcPropertiesForServer   �  ��      |        pcFieldList �  ��      �        pcFieldList �  ��      �        pcFieldList     ��      �        pcFieldList   ��              pcWhere     ��      ,        pcWhere     ��      L        pcState     ��      l       
 phRowObjUpd �  ��      �       
 phRowObj    �  ��      �       
 phRowObj    �  ��      �        phRowObj        ��      �        phRowObj        ��       	        pioCancel       ��      D	        pcRelative      ��      h	       
 phFilterContainer       ��      �	       
 phRowObjUpd �	  ��      �	        pcRowIdent      ��      �	        pcRowIdent      ��       
       
 phAppService        ��      (
        pcMode  T
  ��      H
       
 phSource    x
  ��      l
        phSource        ��      �
       
 phSource    �
  ��      �
        pcText  �
  ��      �
        pcText      ��      �
        pcText     ��             
 phObject    D  ��      8       
 phObject        ��      \        phObject        ��      �        pcField     ��      �        pcCursor    �  ��      �       
 phCaller    �  ��      �        phCaller      ��              phCaller        ��      0        phCaller    \  ��      T        pcMod   |  ��      t        pcMod       ��      �       
 pcMod   �  ��      �       
 phSource    �  ��      �        phSource        ��      �       
 phSource    (  ��               pdRow       ��      @        pdRow       ��      `       
 hTarget �  ��      �        pcMessage       ��      �        pcMessage       ��      �        plEnabled             �     cType       0     V   �                             getObjectType   T	  l	  n	  `        P  
   hReposBuffer    �        t  
   hPropTable  �        �  
   hBuffer           �  
   hTable  �  �     W   <          �                  adm-clone-props q  r  s  t  v  w  x  y  z  {  |  }  �  �  �  �  �            P  
   hProc             p        pcProcName  �  �  	   X   <  X      �                  start-super-proc    �  �  �  �  �  �  �  �  �           �     cTable               iTable  <        0     cColumns    \        P     cDataCols   |        p     cUpdCols    �        �     cCalcData   �        �     cCalcUpd    �     	   �     iNumData    �     
   �     iNumUpd              cBuffers    8        ,     cKeyFields  X        L     cAssignList x        l     iAssigns    �        �     iPos    �        �     iEntry  �        �     iCount  �        �     cTables        �     cTableAssign    ,              cDbEntry    H        @     cField  h        \     cKeyTable   �        |     cQueryString    �        �  
   hQuery  �        �  
   hBuffer �        �     cOpenQuery          �     cDBNames    (             cPhysicalTables T        <     cKeyTableEntityFields   �        h     cKeyTableEntityValues   �        �     cKeyTableEntityMnemonic �         �     cKeyTableEntityObjField �     !   �     cDBName      "        cEntityFields   <     #   ,     lHasObjectField \     $   P     lHasAudit   |     %   p     lHasComment �     &   �     lHasAutoComment �     '   �     iLookup        (   �     iAlias  |    4   Y   �                            initProps   q  r  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �    A  �  �  .  3  5  6  7  :  <  >  ?  B  C  I  W  �            �     lRet                      piTableIndex    �  l  ,   Z   �         X                  deleteRecordStatic  �  �  �  �  �  �  �  �  �  �    	  %  &  B  C  _  `  |  }  �  �  �  �  �  �  �  �      *  +  G  H  d  e  �  �  �  �  �  �  �  �                 !       (  x     [             d                  pushRowObjUpdTable    �        �        pcValType                  $       4        \       |      �                  pushTableAndValidate    4  5  7  0         $        pcContext   H             $       l         `        pcMessages             �        pcUndoIds   �  �     ]             �                  remoteCommit    `  b  f  j  l  �             $          !              pcMessages      !      8        pcUndoIds   �  �     ^       �      t                  serverCommit    �  �  D  �     _               �                  getRowObjUpdStatic  �  �  �       `                                 disable_UI      �  `     a               L                  initializeObject    5  9    d$  
     �      �#                      �  �  �     RowObject   �         �         �         �         �         �         �                                              (         0         8         @         H         P         X         `         h         t         |         �         AlmDes  CodAlm  CodCia  codmat  CodUbi  StkAct  CodCia-2    codfam  CodMar  codmat-2    CodPr1  DesMar  DesMat  TpoArt  UndStk  subfam  FchIng  Pesmat  RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp     �  �     RowObjUpd   �         �         �         �         �         �                                              (         0         8         @         H         P         X         `         h         p         |         �         �         �         AlmDes  CodAlm  CodCia  codmat  CodUbi  StkAct  CodCia-2    codfam  CodMar  codmat-2    CodPr1  DesMar  DesMat  TpoArt  UndStk  subfam  FchIng  Pesmat  RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp ChangedFields   �          �  
   appSrvUtils �        �     s-codcia                  s-codalm    4             xiRocketIndexLimit  \        H  
   gshAstraAppserver   �        p  
   gshSessionManager   �  	 	     �  
   gshRIManager    �  
 
     �  
   gshSecurityManager  �        �  
   gshProfileManager   $            
   gshRepositoryManager    P         8   
   gshTranslationManager   t         d   
   gshWebManager   �         �      gscSessionId    �         �      gsdSessionObj   �         �   
   gshFinManager   !        �   
   gshGenManager   (!        !  
   gshAgnManager   L!        <!     gsdTempUniqueID l!        `!     gsdUserObj  �!        �!     gsdRenderTypeObj    �!        �!     gsdSessionScopeObj  �!       �!  
   ghProp  �!       �!  
   ghADMProps  "       "  
   ghADMPropsBuf   D"       0"     glADMLoadFromRepos  `"       X"     glADMOk �"       t"  
   ghContainer �"    	   �"     cObjectName �"    
   �"     iStart  �"       �"     cAppService �"       �"     cASDivision (#       #     cServerOperatingMode    L#       <#     cContainerType  p#       `#     cQueryString    �#       �#  
   hRowObject  �#       �#  
   hDataQuery  �#       �#     cColumns             �#     cDataFieldDefs  $       $  Almmmatg    ,$        $  Almmmate    H$    X  <$  RowObject         X  X$  RowObjUpd            9   �   �   �   �   "  #  $  %  <  H  I  J  L  N  O  P  T  U  X  Y  Z  [  ]  _  a  c  d  e  h  j  k  m  n  o  p  q  w  y    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  
  E
  F
  H
  I
  J
  K
  L
  M
  O
  P
  Q
  R
  S
  T
  U
  V
  W
  X
  Y
  Z
  [
  \
  ]
  ^
  _
  `
  a
  b
  c
  d
  e
  f
  g
  i
  j
  k
  l
  m
  n
  o
  p
  q
  r
  s
  t
  u
  v
  w
  x
  y
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
                     	  
                �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  1  �  �  �  �  �  �  �  �  �    "  4  S  U  j  �  
    %  5  6  7  :  ;  <  C  D  a  u  �  '  (  4  X  �  �  �  �  �  E  �  �  �  �  �  �  �  E  _  i  �  �  �  �  �  �  �  �  �    /  9  S  u  �  �      ��  C:\Progress\OpenEdge\src\adm2\data.i �(  �) . %C:\Progress\OpenEdge\src\adm2\custom\datacustom.i    �(  �� - C:\Progress\OpenEdge\src\adm2\robjflds.i �(  � , D:\newsie\on_in_co\aplic\alm\dmate-matg-p2.i 0)  �  C:\Progress\OpenEdge\src\adm2\query.i    h)  z + C:\Progress\OpenEdge\src\adm2\delrecst.i �)  `W * C:\Progress\OpenEdge\src\adm2\tblprep.i  �)  F� ) C:\Progress\OpenEdge\gui\fnarg   *   ( %C:\Progress\OpenEdge\src\adm2\custom\querycustom.i   0*  �   C:\Progress\OpenEdge\src\adm2\dataquery.i    p*  �Z ' %C:\Progress\OpenEdge\src\adm2\custom\dataquerycustom.i   �*  �< ! C:\Progress\OpenEdge\src\adm2\appserver.i    �*  �� & %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   $+  I� " C:\Progress\OpenEdge\src\adm2\smart.i    h+  Ds % C:\Progress\OpenEdge\gui\fn  �+  tw $ %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   �+  Q. # C:\Progress\OpenEdge\gui\set ,  �>  C:\Progress\OpenEdge\src\adm2\dataprop.i ,,  ��  %C:\Progress\OpenEdge\src\adm2\custom\datapropcustom.i    `,  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataprtocustom.i    �,  �K  C:\Progress\OpenEdge\src\adm2\qryprop.i  �,  -�  %C:\Progress\OpenEdge\src\adm2\custom\qrypropcustom.i -  ��  %C:\Progress\OpenEdge\src\adm2\custom\qryprtocustom.i \-   	 C:\Progress\OpenEdge\src\adm2\dataqueryprop.i    �-  �d  %C:\Progress\OpenEdge\src\adm2\custom\dataquerypropcustom.i   �-  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataqueryprtocustom.i    .  �l  C:\Progress\OpenEdge\src\adm2\appsprop.i h.  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    �.  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    �.  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i $/  �j  C:\Progress\OpenEdge\gui\get X/  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    �/  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    �/  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i 0  Su  C:\Progress\OpenEdge\src\adm2\globals.i  <0  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i p0  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   �0  �  C:\Progress\OpenEdge\src\adm2\appsprto.i �0  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   (1  ��  C:\Progress\OpenEdge\src\adm2\dataqueryprto.i    p1  ª 
 %C:\Progress\OpenEdge\src\adm2\custom\dataquerydefscustom.i   �1  ��  C:\Progress\OpenEdge\src\adm2\qryprto.i  �1  �  %C:\Progress\OpenEdge\src\adm2\custom\querydefscustom.i   (2  �`  C:\Progress\OpenEdge\src\adm2\dataprto.i l2  �  %C:\Progress\OpenEdge\src\adm2\custom\datadefscustom.i    �2  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   �2  <    D:\newsie\on_in_co\aplic\alm\dmate-matg-p2.w     �   �      T3  [  i     d3     g  %   t3  �   �     �3     �  .   �3  �        �3     `     �3  �   ]     �3     ;  #   �3  �   9     �3       #   �3  �        4     �  #   4  �   �     $4     �  #   44  �   �     D4     �  #   T4  �   �     d4     �  #   t4  �   �     �4     a  #   �4  �   _     �4     =  #   �4  �   0     �4       -   �4  �        �4       ,   �4  k   �     5  �  �     5     �  +   $5  �  �     45     �  +   D5  �  �     T5     g  +   d5  �  d     t5     J  +   �5  �  G     �5     -  +   �5  �  *     �5       +   �5  �       �5     �  +   �5  �  �     �5     �  +   6  �  �     6     �  +   $6  �  �     46     �  +   D6  �  �     T6       +   d6  �  |     t6     b  +   �6  �  _     �6     E  +   �6  �  B     �6     (  +   �6  �  %     �6       +   �6  �       �6     �  +   7  �  �     7     �  +   $7  �  �     47     �  +   D7  �  �     T7     r  #   d7  �  q     t7     O  #   �7  j  *     �7       #   �7  i       �7     �  #   �7  h  �     �7     �  #   �7  ^  �     �7     �  *   8  ]  �     8     k  *   $8  \  j     48     D  *   D8  [  C     T8       *   d8  Z       t8     �  *   �8  Y  �     �8     �  *   �8  X  �     �8     �  *   �8  W  �     �8     �  *   �8  V  �     �8     Z  *   9  U  Y     9     3  *   $9  T  2     49       *   D9  S       T9     �  *   d9  R  �     t9     �  *   �9  Q  �     �9     �  *   �9  P  �     �9     p  *   �9  O  o     �9     I  *   �9  N  H     �9     "  *   :  M  !     :     �  *   $:  ?  �     4:     �  #   D:  	  �     T:     �  )   d:  �   �     t:     `  #   �:  �   _     �:     =  #   �:  �   <     �:       #   �:  �        �:     �  #   �:  �   �     �:     �  #   ;  �   �     ;     �  #   $;  �   A     4;     �  (   D;  g   �     T;  a   �      d;     t  '   t;  _   r      �;     P  #   �;  ]   N      �;     ,  #   �;  I         �;  �     !   �;     �  &   �;  �   �  !   �;     �  #   <  �   �  !   <     m  #   $<  �   k  !   4<     I  #   D<  g   /  !   T<          d<  O   �  !   t<  �   �  "   �<     �  %   �<  �   P  "   �<     �  $   �<  �   �  "   �<     �  #   �<  �   �  "   �<     �  #   �<  �   �  "   =     �  #   =  �   �  "   $=     b  #   4=  �   N  "   D=     ,  #   T=  }      "   d=     �  #   t=     �  "   �=     4  !   �=     �      �=     �     �=     :     �=  �   1     �=  O   #     �=          �=     �     >  �   �     >  �   �     $>  O   t     4>     c     D>          T>  y   �
     d>  �   �
  	   t>  G   �
     �>     �
     �>     }
     �>  c   
  	   �>  x   
     �>  M    
     �>     �	     �>     �	     �>  a   �	     ?  �  k	     ?     L	     $?  �  	     4?  O   	     D?     �     T?     �     d?  �   �     t?     �     �?     �     �?  x   �     �?     �     �?     g     �?     c     �?     O     �?     6     �?  Q   &     @     �     @     �     $@     �     4@     f     D@  ]   `  	   T@     V     d@       	   t@        
   �@     �  	   �@  Z   �     �@     �     �@     �     �@     �     �@     �     �@  c   j     �@     H     A           A     �      $A     �      4A     �      DA     !       TA           