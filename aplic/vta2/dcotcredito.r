	��V�9�a�8  �                                              �G 38AC00EFutf-8 MAIN d:\newsie\on_in_co\APLIC\vta2\dcotcredito.w,, PROCEDURE disable_UI,, PROCEDURE serverCommit,,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE remoteCommit,,INPUT-OUTPUT pcContext CHARACTER,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE pushTableAndValidate,,INPUT pcValType CHARACTER,INPUT-OUTPUT RowObjUpd TABLE PROCEDURE pushRowObjUpdTable,,INPUT RowObjUpd TABLE PROCEDURE initProps,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE viewObject,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE disconnectObject,, PROCEDURE destroyObject,, PROCEDURE bindServer,, PROCEDURE transferDBRow,,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER PROCEDURE startFilter,, PROCEDURE releaseDBRow,, PROCEDURE refetchDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE filterContainerHandler,,INPUT phFilterContainer HANDLE PROCEDURE fetchDBRowForUpdate,, PROCEDURE confirmContinue,,INPUT-OUTPUT pioCancel LOGICAL PROCEDURE compareDBRow,, PROCEDURE bufferCopyDBToRO,,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER PROCEDURE assignDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE updateState,,INPUT pcState CHARACTER PROCEDURE updateQueryPosition,, PROCEDURE updateAddQueryWhere,,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER PROCEDURE undoTransaction,, PROCEDURE transferToExcel,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE synchronizeProperties,,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER PROCEDURE submitValidation,,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER PROCEDURE submitForeignKey,,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER PROCEDURE submitCommit,,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL PROCEDURE startServerObject,, PROCEDURE setPropertyList,,INPUT pcProperties CHARACTER PROCEDURE serverFetchRowObjUpdTable,,OUTPUT phRowObjUpd TABLE-HANDLE PROCEDURE serverSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE PROCEDURE saveContextAndDestroy,,OUTPUT pcContext CHARACTER PROCEDURE rowObjectState,,INPUT pcState CHARACTER PROCEDURE retrieveFilter,, PROCEDURE restartServerObject,, PROCEDURE remoteSendRows,,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE,OUTPUT pocMessages CHARACTER PROCEDURE refreshRow,, PROCEDURE printToCrystal,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE isUpdatePending,,INPUT-OUTPUT plUpdate LOGICAL PROCEDURE initializeServerObject,, PROCEDURE initializeObject,, PROCEDURE home,, PROCEDURE genContextList,,OUTPUT pcContext CHARACTER PROCEDURE fetchPrev,, PROCEDURE fetchNext,, PROCEDURE fetchLast,, PROCEDURE fetchFirst,, PROCEDURE fetchBatch,,INPUT plForwards LOGICAL PROCEDURE endClientDataRequest,, PROCEDURE destroyServerObject,, PROCEDURE describeSchema,,INPUT pcSdoName CHARACTER,OUTPUT hTtSchema TABLE-HANDLE PROCEDURE dataAvailable,,INPUT pcRelative CHARACTER PROCEDURE copyColumns,,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE PROCEDURE commitTransaction,, PROCEDURE clientSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER PROCEDURE batchServices,,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER FUNCTION deleteRecordStatic,logical,INPUT piTableIndex INTEGER FUNCTION getRowObjUpdStatic,widget-handle, FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION whereClauseBuffer,CHARACTER,INPUT pcWhere CHARACTER FUNCTION setQueryWhere,LOGICAL,INPUT pcWhere CHARACTER FUNCTION setQueryString,LOGICAL,INPUT pcQueryString CHARACTER FUNCTION setQuerySort,LOGICAL,INPUT pcSort CHARACTER FUNCTION setQueryPosition,LOGICAL,INPUT pcPosition CHARACTER FUNCTION rowidWhereCols,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION rowidWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION resolveBuffer,CHARACTER,INPUT pcBuffer CHARACTER FUNCTION removeQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER FUNCTION removeForeignKey,LOGICAL, FUNCTION refreshRowident,CHARACTER,INPUT pcRowident CHARACTER FUNCTION newWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION newQueryValidate,CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryString,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER FUNCTION insertExpression,CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER FUNCTION indexInformation,CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER FUNCTION getTargetProcedure,HANDLE, FUNCTION getQueryWhere,CHARACTER, FUNCTION getQueryString,CHARACTER, FUNCTION getQuerySort,CHARACTER, FUNCTION getQueryPosition,CHARACTER, FUNCTION getForeignValues,CHARACTER, FUNCTION getDataColumns,CHARACTER, FUNCTION excludeColumns,CHARACTER,INPUT iTable INTEGER FUNCTION dbColumnHandle,HANDLE,INPUT pcColumn CHARACTER FUNCTION dbColumnDataName,CHARACTER,INPUT pcDbColumn CHARACTER FUNCTION columnValMsg,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnTable,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnQuerySelection,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDbColumn,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDataType,CHARACTER,INPUT pcColumn CHARACTER FUNCTION bufferWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER FUNCTION bufferCompareDBToRO,LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER FUNCTION assignQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION addQueryWhere,LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION getObjectType,character, FUNCTION updateRow,LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER FUNCTION submitRow,LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER FUNCTION rowValues,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION rowAvailable,LOGICAL,INPUT pcDirection CHARACTER FUNCTION prepareQuery,LOGICAL,INPUT pcQuery CHARACTER FUNCTION openQuery,LOGICAL, FUNCTION openDataQuery,LOGICAL,INPUT pcPosition CHARACTER FUNCTION hasForeignKeyChanged,LOGICAL, FUNCTION getLastCommitErrorType,CHARACTER, FUNCTION firstRowIds,CHARACTER,INPUT pcQueryString CHARACTER FUNCTION findRowWhere,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION findRow,LOGICAL,INPUT pcKeyValues CHARACTER FUNCTION fetchRowIdent,CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER FUNCTION fetchRow,CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER FUNCTION deleteRow,LOGICAL,INPUT pcRowIdent CHARACTER FUNCTION createRow,LOGICAL,INPUT pcValueList CHARACTER FUNCTION copyRow,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION colValues,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION columnProps,CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER FUNCTION closeQuery,LOGICAL, FUNCTION canNavigate,LOGICAL, FUNCTION cancelRow,CHARACTER, FUNCTION addRow,CHARACTER,INPUT pcViewColList CHARACTER TEMP-TABLE RowObjUpd 0,RowNum:RowNum 0 NO,Atencion character 0 0,Cmpbnte character 1 0,CodCia integer 2 0,CodCli character 3 0,CodDiv character 4 0,CodDoc character 5 0,CodMon integer 6 0,CodPos character 7 0,CodVen character 8 0,DirCli character 9 0,FchEnt date 10 0,FchPed date 11 0,fchven date 12 0,FlgEst character 13 0,FlgIgv logical 14 0,FmaPgo character 15 0,Glosa character 16 0,LugEnt character 17 0,NomCli character 18 0,NroPed character 19 0,ordcmp character 20 0,RucCli character 21 0,Sede character 22 0,TpoCmb decimal 23 0,usuario character 24 0,RowNum integer 25 0,RowIdent character 26 0,RowMod character 27 0,RowIdentIdx character 28 0,RowUserProp character 29 0,ChangedFields character 30 0      0<              |(             � 0<  L�              <�              �A     +   �� �  W   X� `  X   ��   Y   ��   [   ��   \   �� <  ]   0�    ^   P� 0  `   ? �� @   iSO8859-1                                                                           p;    �                                      �                   �                �;      H   Z   T�              ��  �   �;      <          �                                             PROGRESS                         �           
    
                    �              �                                                                                                     
               INTEGRAL                         PROGRESS                                 �                                ��{a               ��                              �  l                      �  |  #�     CODCIACODDOCNROPEDFCHPEDCODCLINOMCLIDIRCLIRUCCLIHORAFLGESTCODALMCODMONTPOCMBUSUARIOGLOSAIMPBRTIMPEXOPORIGVIMPDTOIMPTOTIMPIGVCODVENIMPISCIMPVTAFLGSITFMAPGOFCHVENORDCMPCODDIVTIPVTAPORDTOTPOPEDUSRDSCTOLUGENTCODTRANSNROREFCMPBNTEOBSERVANCMPBNTEATENCIONFAXCLILUGENT2IMPFLEFLGIGVTPOLICUBIGEOACUBONNROCARDTIPBONIMPORTEPORCENTUSRAPROBACIONFCHAPROBACIONFCHENTCODPOSFLGENVUSRSACFECSACHORSACLIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02USRCHQFCHCHQHORCHQSEDEDIVDESCODREFIMPDTO2GLOSAIMPDTO2FLGIMPODUSRIMPODFCHIMPODUSRACTFECACTHORACTMOTREPOSICIONVTAPUNTUALCROSSDOCKINGALMACENXDCODORIGENNROORIGENDTALMACENDTEMPAQESPECCLIENTE_RECOGELISTA_DE_PRECIOSCUSTOMERPURCHASEORDERCUSTOMERREQUESTORDERTYPEPERIODCURRENCYPAYFORMDELIVERYDATEREGION1REGION1NAMEREGION2REGION2NAMEREGION3REGION3NAMETELEPHONECONTACTRECEPTORCONTACTRECEPTORNAMEDELIVERYADDRESSCUSTOMERLABELOFFICECUSTOMEROFFICECUSTOMERNAMECUSTOMERSTOCKDEPOCUSTOMERSTOCKDEPONAMECONSOLIDATEINVOICECUSTOMERINVOICECUSTOMERGROUPITEMSPESOVOLUMENEMBALAJE_ESPECIALDELIVERYGROUPDNICLIE-MAILIMPPERCEPCIONPORPERCEPCIONCODPAISCODDEPTCODPROVCODDISTREFERENCEADDRESSIDCUSTOMERFLAGMIGRACIONMIGFECHAMIGHORAMIGUSUARIOTOTALVALORVENTANETOOPGRAVADASTOTALVALORVENTANETOOPGRATUITASTOTALTRIBUTOSOPEGRATUITASTOTALIGVTOTALIMPUESTOSTOTALVALORVENTATOTALPRECIOVENTADESCUENTOSGLOBALESPORCENTAJEDSCTOGLOBALMONTOBASEDESCUENTOGLOBALTOTALVALORVENTANETOOPNOGRAVADATOTALDOCUMENTOANTICIPOMONTOBASEDSCTOGLOBALANTICIPOPORCENTAJEDSCTOGLOBALANTICIPOTOTALDSCTOGLOBALESANTICIPOMONTOBASEICBPERTOTALMONTOICBPERTOTALVALORVENTANETOOPEXONERADASTOTALVENTA                                                                       	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /         0 
        1          2 
        3         4         5          6          7          8          9          :          ;          <          =          >          ?          @          A          B          C          D          E          F          G          H          I          J          K          L          M          N          O          P   "       Q          R          S          T          U          V          W          X          Y          Z          [          \          ]          ^          _          `          a          b          d          e          f          g          h          i          j          k          l          m          n          o          p          q          r          s          t          u          v          w          x          y          z          {          |          }          ~                    �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �           
    
                  �  �             p                                                                                                    
  0         �  
    
                  �  `                                                                                                                  
  �  2      X  
    
                  D               �                                                                                          2          
  �  ?        
    
                  �  �             t                                                                                          ?          
  4  R      �  
    
                  �  d                                                                                                        R          
  �  d      \  
    
                  H    	           �                                                                                          d          
  �  y        
    
                  �  �  
           x                                                                                          y          
  8  �      �  
    
                  �  h             $                                                                                          �          
  �  �      `                         L               �                                                                                          �            �  �                              �  �             |                                                                                          �            <  �      �  
    
                  �  l             (                                                                                          �          
  �  �      d  
    
                  P               �                                                                                          �          
  �  �        
    
                  �  �             �                                                                                          �          
  @  �      �                        �  p             ,                                                                                          �            �  �      h                        T               �                                                                                          �            �  �                                 �             �                                                                                          �                      �                        �  D             0                                                                                                      �         �       �  X  �+  ?   �+  �  M!       ,         �             4          �       �              �       �  X  T:  B   �:  �  �6      �:         �         �    �,          /      �                 h�                                               l�          �  �  L l|                �         
             
             
                                         
                                                                                                               
             
                                          L   \   l   |   �   �   �   �   �   �   �   �       ,  <  L  \      L   \   l   |   �   �   �   �   �   �   �   �      ,  <  L  \                                                                                                                                     	                  
                                                                                                                                                                                                                                                                                                                                                                                                                           �%   &  &   &  &                         $&  ,&  4&  \&  H&                         `&  h&  l&  t&  p&          x&             �&  �&  �&  �&  �&          �&             �&  �&  �&  �&  �&          �&             '  '  $'  4'  ,'                         8'  @'  D'  T'  L'          X'             l'  t'  |'  �'                             �'  �'  �'  �'  �'                         �'  �'  �'  �'  �'          �'             �'   (  (  (                             $(  ,(  8(  L(  @(                         T(  \(  h(  �(  |(                         �(  �(  �(  �(  �(                         �(  �(  �(  �(  �(                         �(  �(  �(  )  �(                         )  )  $)  4)  ,)                         8)  @)  H)  p)  \)                         t)  |)  �)  �)  �)          �)             �)  �)  �)  �)  �)                         �)  �)  �)  *  �)                         *  *   *  (*  $*          ,*             X*  `*  h*  p*                             t*  |*  �*  �*  �*          �*             �*  �*  �*  �*  �*                         �*  �*  �*  �*                             �*  +  +  +                               +  (+  0+  8+                             <+  H+  P+  \+                             `+  l+  t+  �+                                                                          Atencion    X(30)   Atenci�n    Atenci�n        Cmpbnte X(3)    Tipo Comprobante    Tipo Comprobante    FAC CodCia  999 Cia Cia 0   C�digo de Compa�ia  CodCli  x(11)   Codigo  Codigo      C�digo del Cliente  CodDiv  x(5)    Division    Division    00000   Ingrese el Codigo de Division   CodDoc  x(3)    Codigo  Codigo      CodMon  9   Moneda  Cod!mon 1   C�digo de moneda    CodPos  x(3)    Postal      CodVen  x(10)   Vendedor    Vendedor        DirCli  x(100)  Direcci�n   Direcci�n       Direcci�n del Cliente   FchEnt  99/99/9999  Fecha Entrega   TODAY   FchPed  99/99/9999  Fecha   Fch.Pedido  today   fchven  99/99/99    Fecha Vencimiento   Fecha!Vcmto.    ?   FlgEst  X(1)    Estado  Estado  P   FlgIgv  Si/no   Con IGV Con IGV Si  FmaPgo  X(8)    Condicion de ventas Condicion de!venta      Glosa   X(50)   Glosa   Glosa       LugEnt  x(60)   Lugar de entrega    Lugar de entrega        NomCli  x(100)  Nombre  Nombre      Nombre del Cliente  NroPed  X(12)   No. Pedido  Numero!Pedido       ordcmp  X(12)   Orden de Compra Orden ! Compra      RucCli  x(20)   Ruc Ruc     Registro Unico de Contribuyente (Cliente)   Sede    x(5)    Sede        TpoCmb  Z,ZZ9.9999  Tipo de cambio  T/Cambio    0   Tipo de cambio  usuario x(10)   usuario usuario     RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     �  )�  ��������� FAC  00000    ���P                    �        �        �                �     i     i     i     	 	 	      &  .  5  <  C  J  Q  X  _  f  m  t  {  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �                                                                                                                                     	                  
                                                                                                                                                                                                                                                                                                                                                                                                                                              �4  �4  �4  �4  �4                         �4  �4  �4   5  �4                         5  5  5  5  5          5             05  85  @5  P5  H5          T5             h5  p5  x5  �5  �5          �5             �5  �5  �5  �5  �5                         �5  �5  �5  �5  �5          �5             6  6   6  (6                             ,6  46  <6  T6  H6                         X6  `6  h6  �6  t6          �6             �6  �6  �6  �6                             �6  �6  �6  �6  �6                         �6   7  7  07   7                         47  <7  D7  T7  L7                         X7  `7  h7  x7  p7                         |7  �7  �7  �7  �7                         �7  �7  �7  �7  �7                         �7  �7  �7  8   8                         8   8  (8  88  08          <8             P8  X8  `8  |8  l8                         �8  �8  �8  �8  �8                         �8  �8  �8  �8  �8          �8             �8  9  9  9                             9   9  ,9  H9  <9          L9             \9  d9  l9  |9  t9                         �9  �9  �9  �9                             �9  �9  �9  �9                              �9  �9  �9  �9                             �9  �9  �9   :                             :  :  :  $:                              (:  8:  @:  P:                                                                          Atencion    X(30)   Atenci�n    Atenci�n        Cmpbnte X(3)    Tipo Comprobante    Tipo Comprobante    FAC CodCia  999 Cia Cia 0   C�digo de Compa�ia  CodCli  x(11)   Codigo  Codigo      C�digo del Cliente  CodDiv  x(5)    Division    Division    00000   Ingrese el Codigo de Division   CodDoc  x(3)    Codigo  Codigo      CodMon  9   Moneda  Cod!mon 1   C�digo de moneda    CodPos  x(3)    Postal      CodVen  x(10)   Vendedor    Vendedor        DirCli  x(100)  Direcci�n   Direcci�n       Direcci�n del Cliente   FchEnt  99/99/9999  Fecha Entrega   TODAY   FchPed  99/99/9999  Fecha   Fch.Pedido  today   fchven  99/99/99    Fecha Vencimiento   Fecha!Vcmto.    ?   FlgEst  X(1)    Estado  Estado  P   FlgIgv  Si/no   Con IGV Con IGV Si  FmaPgo  X(8)    Condicion de ventas Condicion de!venta      Glosa   X(50)   Glosa   Glosa       LugEnt  x(60)   Lugar de entrega    Lugar de entrega        NomCli  x(100)  Nombre  Nombre      Nombre del Cliente  NroPed  X(12)   No. Pedido  Numero!Pedido       ordcmp  X(12)   Orden de Compra Orden ! Compra      RucCli  x(20)   Ruc Ruc     Registro Unico de Contribuyente (Cliente)   Sede    x(5)    Sede        TpoCmb  Z,ZZ9.9999  Tipo de cambio  T/Cambio    0   Tipo de cambio  usuario x(10)   usuario usuario     RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     ChangedFields   x(8)    ChangedFields       �  ) :�  ��� ������ FAC  00000    ���P                      �        �        �                �     i     i     i     	 	 	      &  .  5  <  C  J  Q  X  _  f  m  t  {  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �    ��                            ����                            �    ��                        5    ��                    ��    undefined                                                               �       ��  �   l   ��  ��                    �����               $�b                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D       �     9          ��    �   �  4      d       4   ����d                 D                      ��                  �   �                   \�b                       �   �  �  	  �   x                                        3   ����|       O   �   ��  ��  �   batchServices                               4        ��                  V  Y  L              �d                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             d               ��                  �           ��                            ����                            clientSendRows                              �  p      ��                  [  a  �              ��c                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                �               ��   <                            ��   d             0               ��                  X           ��                            ����                            commitTransaction                               X  @      ��                  c  d  p              �d                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            copyColumns                             X  @      ��                  f  i  p              �d                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               �� 
                 �  
         ��                            ����                            dataAvailable                               �  �      ��                  k  m  �              X_a                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            describeSchema                              �	  �	      ��                  o  r  �	              \�d                    O   ����    e�          O   ����    R�          O   ����    ��            ��   <
             
               �� 
          �       0
  
         ��                            ����                            destroyServerObject                             0        ��                  t  u  H              t�b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            endClientDataRequest                                <  $      ��                  w  x  T              �b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchBatch                              <  $      ��                  z  |  T              ̥b                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  l           ��                            ����                            fetchFirst                              d  L      ��                  ~    |              �{c                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               d  L      ��                  �  �  |              �jc                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               d  L      ��                  �  �  |              4kc                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               d  L      ��                  �  �  |              �	a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            genContextList                              h  P      ��                  �  �  �              
a                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            home                                �  t      ��                  �  �  �              �
a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �  |      ��                  �  �  �              a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �  �      ��                  �  �  �              ��c                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            isUpdatePending                             �  �      ��                  �  �  �              t�c                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            printToCrystal                              �  �      ��                  �  �  �              �a                    O   ����    e�          O   ����    R�          O   ����    ��            ��   4                             ��   \             (               ��                  P           ��                            ����                            refreshRow                              H  0      ��                  �  �  `              �Mb                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            remoteSendRows                              L  4      ��                  �  �  d              LLd                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             |               ��   �             �               ��                 �               ��   (             �               ��   P                            ��   x             D               �� 
  �      �       l  
             ��                  �           ��                            ����                            restartServerObject                             �  |      ��                  �  �  �              <�c                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            retrieveFilter                              �  �      ��                  �  �  �              p�b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            rowObjectState                              �  �      ��                  �  �  �              Cc                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            saveContextAndDestroy                               �  �      ��                  �  �  �              )b                    O   ����    e�          O   ����    R�          O   ����    ��            ��                               ��                            ����                            serverSendRows                              �   �       ��                  �  �  !              سc                    O   ����    e�          O   ����    R�          O   ����    ��            ��   `!             ,!               ��   �!             T!               ��   �!             |!               ��   �!             �!               ��    "             �!               �� 
          �       �!  
         ��                            ����                            serverFetchRowObjUpdTable                               �"  �"      ��                  �  �  #              �)a                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
          �       ,#  
         ��                            ����                            setPropertyList                             ($  $      ��                  �  �  @$              (Ja                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  X$           ��                            ����                            serverSendRows                              T%  <%      ��                  �  �  l%              P�d                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �%             �%               ��   �%             �%               ��   &             �%               ��   0&             �%               ��   X&             $&               �� 
          �       L&  
         ��                            ����                            startServerObject                               L'  4'      ��                  �  �  d'              (�a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            submitCommit                                P(  8(      ��                  �  �  h(              �b                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �(             �(               ��                  �(           ��                            ����                            submitForeignKey                                �)  �)      ��                  �  �  �)              �a                    O   ����    e�          O   ����    R�          O   ����    ��            ��   *             �)               ��   4*              *               ��                  (*           ��                            ����                            submitValidation                                (+  +      ��                  �  �  @+              *c                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �+             X+               ��                  �+           ��                            ����                            synchronizeProperties                               �,  l,      ��                  �  �  �,              ��c                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �,             �,               ��                  �,           ��                            ����                            transferToExcel                             �-  �-      ��                      �-              Dud                    O   ����    e�          O   ����    R�          O   ����    ��            ��   <.             .               ��   d.             0.               ��   �.             X.               ��                  �.           ��                            ����                            undoTransaction                             |/  d/      ��                      �/              ,�d                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateAddQueryWhere                             �0  l0      ��                      �0              ��d                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �0             �0               ��                  �0           ��                            ����                            updateQueryPosition                             �1  �1      ��                      �1              ��d                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateState                             �2  �2      ��                      �2              d�b                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  3           ��                            ����                            addRow          t3      �3     w       CHARACTER,INPUT pcViewColList CHARACTER cancelRow   |3      �3      �3   	 ~       CHARACTER,  canNavigate �3      �3      (4    �       LOGICAL,    closeQuery  4      44      `4   
 �       LOGICAL,    columnProps @4      l4      �4    �       CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER  colValues   x4      �4      5   	 �       CHARACTER,INPUT pcViewColList CHARACTER copyRow �4      ,5      T5    �       CHARACTER,INPUT pcViewColList CHARACTER createRow   45      |5      �5   	 �       LOGICAL,INPUT pcValueList CHARACTER deleteRow   �5      �5      �5   	 �       LOGICAL,INPUT pcRowIdent CHARACTER  fetchRow    �5      6      H6  	  �       CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER fetchRowIdent   (6      �6      �6  
  �       CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER  findRow �6      �6       7    �       LOGICAL,INPUT pcKeyValues CHARACTER findRowWhere     7      D7      t7    �       LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  firstRowIds T7      �7      �7    �       CHARACTER,INPUT pcQueryString CHARACTER getLastCommitErrorType  �7       8      X8    	      CHARACTER,  hasForeignKeyChanged    88      d8      �8           LOGICAL,    openDataQuery   |8      �8      �8    5      LOGICAL,INPUT pcPosition CHARACTER  openQuery   �8      �8      (9   	 C      LOGICAL,    prepareQuery    9      49      d9    M      LOGICAL,INPUT pcQuery CHARACTER rowAvailable    D9      �9      �9    Z      LOGICAL,INPUT pcDirection CHARACTER rowValues   �9      �9      :   	 g      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER    submitRow   �9      `:      �:   	 q      LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER  updateRow   l:      �:      �:   	 {      LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER getObjectType   �:      8;      h;    �      CHARACTER,  assignDBRow                             <  �;      ��                      <              �lb                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 4<  
         ��                            ����                            bufferCopyDBToRO                                4=  =      ��                      L=              <mb                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �=             d=  
             �� 
  �=             �=  
             ��   �=             �=               ��                  �=           ��                            ����                            compareDBRow                                �>  �>      ��                      �>              �d                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmContinue                             �?  �?      ��                      �?              p�d                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  @           ��                            ����                            dataAvailable                               A  �@      ��                       A              �b                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  8A           ��                            ����                            fetchDBRowForUpdate                             8B   B      ��                      PB              �da                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchFirst                              8C   C      ��                      PC              tga                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               8D   D      ��                      PD              �nc                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               8E   E      ��                  !  "  PE              �oc                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               8F   F      ��                  $  %  PF              ��a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            filterContainerHandler                              DG  ,G      ��                  '  )  \G              ��c                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 tG  
         ��                            ����                            initializeObject                                tH  \H      ��                  +  ,  �H              D<d                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            refetchDBRow                                xI  `I      ��                  .  0  �I              d=d                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �I  
         ��                            ����                            releaseDBRow                                �J  �J      ��                  2  3  �J              `�a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            startFilter                             �K  �K      ��                  5  6  �K              ��a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            transferDBRow                               �L  �L      ��                  8  ;  �L              ��a                    O   ����    e�          O   ����    R�          O   ����    ��            ��   M             �L               ��                   M           ��                            ����                            addQueryWhere   H;      hM      �M    �      LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER    assignQuerySelection    xM      �M      $N    �      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  bufferCompareDBToRO N      |N      �N    �      LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER bufferWhereClause   �N      O      PO    �      CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER  columnDataType  0O      �O      �O    �      CHARACTER,INPUT pcColumn CHARACTER  columnDbColumn  �O      �O      P    �      CHARACTER,INPUT pcColumn CHARACTER  columnQuerySelection    �O      4P      lP          CHARACTER,INPUT pcColumn CHARACTER  columnTable LP      �P      �P    "      CHARACTER,INPUT pcColumn CHARACTER  columnValMsg    �P      �P      Q     .      CHARACTER,INPUT pcColumn CHARACTER  dbColumnDataName    �P      4Q      hQ  !  ;      CHARACTER,INPUT pcDbColumn CHARACTER    dbColumnHandle  HQ      �Q      �Q  "  L      HANDLE,INPUT pcColumn CHARACTER excludeColumns  �Q      �Q      R  #  [      CHARACTER,INPUT iTable INTEGER  getDataColumns  �Q      0R      `R  $  j      CHARACTER,  getForeignValues    @R      lR      �R  %  y      CHARACTER,  getQueryPosition    �R      �R      �R  &  �      CHARACTER,  getQuerySort    �R      �R      S  '  �      CHARACTER,  getQueryString  �R      (S      XS  (  �      CHARACTER,  getQueryWhere   8S      dS      �S  )  �      CHARACTER,  getTargetProcedure  tS      �S      �S  *  �      HANDLE, indexInformation    �S      �S      T  +  �      CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER   insertExpression    �S      lT      �T  ,  �      CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER  newQueryString  �T      �T      (U  -  �      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER  newQueryValidate    U      �U      �U  .  	      CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER   newQueryWhere   �U      dV      �V  /        CHARACTER,INPUT pcWhere CHARACTER   newWhereClause  tV      �V      �V  0  (      CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER refreshRowident �V      XW      �W  1  7      CHARACTER,INPUT pcRowident CHARACTER    removeForeignKey    hW      �W      �W  2  G      LOGICAL,    removeQuerySelection    �W      �W      (X  3  X      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER   resolveBuffer   X      hX      �X  4  m      CHARACTER,INPUT pcBuffer CHARACTER  rowidWhere  xX      �X      �X  5 
 {      CHARACTER,INPUT pcWhere CHARACTER   rowidWhereCols  �X      Y      <Y  6  �      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER    setQueryPosition    Y      �Y      �Y  7  �      LOGICAL,INPUT pcPosition CHARACTER  setQuerySort    �Y      �Y       Z  8  �      LOGICAL,INPUT pcSort CHARACTER  setQueryString   Z      @Z      pZ  9  �      LOGICAL,INPUT pcQueryString CHARACTER   setQueryWhere   PZ      �Z      �Z  :  �      LOGICAL,INPUT pcWhere CHARACTER whereClauseBuffer   �Z      �Z      [  ;  �      CHARACTER,INPUT pcWhere CHARACTER   bindServer                              �[  �[      ��                  �  �  �[              �d                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �\  �\      ��                  �  �  �\               d                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �]  �]      ��                  �  �  �]              �d                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �^  �^      ��                  �  �  �^              ��b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �_  �_      ��                  �  �  `              ��b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �`  �`      ��                  �  �  a              ��d                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �a  �a      ��                  �  �  b              �d                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 ,b  
         ��                            ����                            startServerObject                               ,c  c      ��                  �  �  Dc              |�b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                0d  d      ��                  �  �  Hd              ��b                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  `d           ��                            ����                            getAppService   �Z      �d      �d  <  �      CHARACTER,  getASBound  �d      e      0e  = 
 �      LOGICAL,    getAsDivision   e      <e      le  >  �      CHARACTER,  getASHandle Le      xe      �e  ?  	      HANDLE, getASHasStarted �e      �e      �e  @        LOGICAL,    getASInfo   �e      �e      f  A 	 %      CHARACTER,  getASInitializeOnRun    �e       f      Xf  B  /      LOGICAL,    getASUsePrompt  8f      df      �f  C  D      LOGICAL,    getServerFileName   tf      �f      �f  D  S      CHARACTER,  getServerOperatingMode  �f      �f      g  E  e      CHARACTER,  runServerProcedure  �f      $g      Xg  F  |      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   8g      �g      �g  G  �      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   �g      �g      $h  H  �      LOGICAL,INPUT pcDivision CHARACTER  setASHandle h      Hh      th  I  �      LOGICAL,INPUT phASHandle HANDLE setASInfo   Th      �h      �h  J 	 �      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    �h      �h      i  K  �      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �h      <i      li  L  �      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   Li      �i      �i  M  �      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  �i      �i      j  N  �      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �j  �j      ��                  �  �  �j              a                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  <k             k  
             ��   dk             0k               �� 
                 Xk  
         ��                            ����                            addMessage                              Pl  8l      ��                  �  �  hl               �a                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �l             �l               ��   �l             �l               ��                  �l           ��                            ����                            adjustTabOrder                              �m  �m      ��                  �  �  �m              �$c                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  0n             �m  
             �� 
  Xn             $n  
             ��                  Ln           ��                            ����                            applyEntry                              Do  ,o      ��                  �  �  \o               c                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  to           ��                            ����                            changeCursor                                pp  Xp      ��                  �  �  �p              0
b                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �p           ��                            ����                            createControls                              �q  �q      ��                  �  �  �q              �c                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �r  �r      ��                  �  �  �r              ��c                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �s  �s      ��                  �  �  �s              зc                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �t  �t      ��                  �  �  �t              ��a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �u  �u      ��                  �  �  �u              �a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �v  �v      ��                  �  �  �v              ȓa                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �w  �w      ��                  �  �  �w              H\b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              �x  �x      ��                  �  �  �x              P]b                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  $y             �x  
             ��   Ly             y               ��   ty             @y               ��                  hy           ��                            ����                            modifyUserLinks                             dz  Lz      ��                  �  �  |z               �c                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �z             �z               ��   �z             �z               �� 
                 �z  
         ��                            ����                            removeAllLinks                              �{  �{      ��                  �  �  �{              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              �|  �|      ��                  �  �  �|              p                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  D}             }  
             ��   l}             8}               �� 
                 `}  
         ��                            ����                            repositionObject                                `~  H~      ��                  �  �  x~              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �~             �~               ��                  �~           ��                            ����                            returnFocus                             �  �      ��                  �  �  �              8�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �  
         ��                            ����                            showMessageProcedure                                �  ̀      ��                      ��              T�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   H�             �               ��                  <�           ��                            ����                            toggleData                              4�  �      ��                      L�              <�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  d�           ��                            ����                            viewObject                              \�  D�      ��                  
    t�              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �i      ̃      ��  O 
 \      LOGICAL,    assignLinkProperty  ؃      �      8�  P  g      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   �      ��      ��  Q  z      CHARACTER,  getChildDataKey ��      ̄      ��  R  �      CHARACTER,  getContainerHandle  ܄      �      <�  S  �      HANDLE, getContainerHidden  �      D�      x�  T  �      LOGICAL,    getContainerSource  X�      ��      ��  U  �      HANDLE, getContainerSourceEvents    ��      ��      ��  V  �      CHARACTER,  getContainerType    ܅      �      <�  W  �      CHARACTER,  getDataLinksEnabled �      H�      |�  X  �      LOGICAL,    getDataSource   \�      ��      ��  Y        HANDLE, getDataSourceEvents ��      ��      �  Z        CHARACTER,  getDataSourceNames  Ԇ       �      4�  [  1      CHARACTER,  getDataTarget   �      @�      p�  \  D      CHARACTER,  getDataTargetEvents P�      |�      ��  ]  R      CHARACTER,  getDBAware  ��      ��      �  ^ 
 f      LOGICAL,    getDesignDataObject ȇ      �      (�  _  q      CHARACTER,  getDynamicObject    �      4�      h�  `  �      LOGICAL,    getInstanceProperties   H�      t�      ��  a  �      CHARACTER,  getLogicalObjectName    ��      ��      ��  b  �      CHARACTER,  getLogicalVersion   Ј      ��      0�  c  �      CHARACTER,  getObjectHidden �      <�      l�  d  �      LOGICAL,    getObjectInitialized    L�      x�      ��  e  �      LOGICAL,    getObjectName   ��      ��      �  f  �      CHARACTER,  getObjectPage   ̉      ��      (�  g        INTEGER,    getObjectParent �      4�      d�  h        HANDLE, getObjectVersion    D�      l�      ��  i  $      CHARACTER,  getObjectVersionNumber  ��      ��      �  j  5      CHARACTER,  getParentDataKey    Ċ      ��      $�  k  L      CHARACTER,  getPassThroughLinks �      0�      d�  l  ]      CHARACTER,  getPhysicalObjectName   D�      p�      ��  m  q      CHARACTER,  getPhysicalVersion  ��      ��      �  n  �      CHARACTER,  getPropertyDialog   ȋ      �      (�  o  �      CHARACTER,  getQueryObject  �      4�      d�  p  �      LOGICAL,    getRunAttribute D�      p�      ��  q  �      CHARACTER,  getSupportedLinks   ��      ��      ��  r  �      CHARACTER,  getTranslatableProperties   ��      �      (�  s  �      CHARACTER,  getUIBMode  �      4�      `�  t 
 �      CHARACTER,  getUserProperty @�      l�      ��  u  	      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    |�      č      ��  v  	      CHARACTER,INPUT pcPropList CHARACTER    linkHandles ܍      $�      P�  w  '	      CHARACTER,INPUT pcLink CHARACTER    linkProperty    0�      t�      ��  x  3	      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry ��      ��      �  y  @	      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   �      x�      ��  z  L	      CHARACTER,INPUT piMessage INTEGER   propertyType    ��      ̏      ��  {  Z	      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  ܏      $�      T�  |  g	      CHARACTER,  setChildDataKey 4�      `�      ��  }  v	      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  p�      ��      �  ~  �	      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  ̐      �      @�    �	      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents     �      `�      ��  �  �	      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled |�      ��      ��  �  �	      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   ԑ      �      L�  �  �	      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents ,�      l�      ��  �  �	      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  ��      Ȓ      ��  �  �	      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   ܒ      $�      T�  �  
      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents 4�      x�      ��  �  
      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  ��      Г      ��  � 
 0
      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject ܓ      �      P�  �  ;
      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    0�      x�      ��  �  O
      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   ��      Ȕ       �  �  `
      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    ��      $�      \�  �  v
      LOGICAL,INPUT c CHARACTER   setLogicalVersion   <�      x�      ��  �  �
      LOGICAL,INPUT cVersion CHARACTER    setObjectName   ��      Е       �  �  �
      LOGICAL,INPUT pcName CHARACTER  setObjectParent ��       �      P�  �  �
      LOGICAL,INPUT phParent HANDLE   setObjectVersion    0�      p�      ��  �  �
      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    ��      ̖       �  �  �
      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks ��      (�      \�  �  �
      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   <�      |�      ��  �  �
      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  ��      ԗ      �  �        LOGICAL,INPUT cVersion CHARACTER    setRunAttribute �      ,�      \�  �        LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   <�      ��      ��  �  *      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   ��      ܘ      �  �  <      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  ��      <�      h�  � 
 V      LOGICAL,INPUT pcMode CHARACTER  setUserProperty H�      ��      ��  �  a      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ��      ��      $�  �  q      LOGICAL,INPUT pcMessage CHARACTER   Signature   �      H�      t�  � 	 }      CHARACTER,INPUT pcName CHARACTER    l�    !  ��  0�      �       4   �����                 @�                      ��                  "  O                  $3                       "  Ě        #  \�  ؛      �       4   �����                 �                      ��                  $  N                  �3                       $  l�  �    ;  �  ��      �       4   �����                 ��                      ��                  G  I                  �                       G  �         H                                  ,     
                    � ߱        �  $  K  ��  ���                           $  M  @�  ���                       x                         � ߱        x�    S  ��  �      �      4   �����                �                      ��                  T  	                  �	                       T  ��  H�  o   W      ,                                 ��  $   X  t�  ���                       �  @         �              � ߱        ��  �   Y        Ȟ  �   Z  �      ܞ  �   \        �  �   ^  x      �  �   `  �      �  �   b  `      ,�  �   c  �      @�  �   d        T�  �   g  �      h�  �   i         |�  �   j  |      ��  �   l  �      ��  �   m  t      ��  �   n  �      ̟  �   o  ,      ��  �   p  �      ��  �   v  �      �  �   x  P	      �  �   ~  �	      0�  �   �   
      D�  �   �  t
      X�  �   �  �
      l�  �   �  l      ��  �   �  �      ��  �   �  \      ��  �   �  �      ��  �   �  D      Р  �   �  �      �  �   �  �      ��  �   �  0      �  �   �  �       �  �   �  �      4�  �   �        H�  �   �  X      \�  �   �  �      p�  �   �        ��  �   �  L      ��  �   �  �      ��  �   �  �      ��  �   �         ԡ  �   �  <      �  �   �  x      ��  �   �  �      �  �   �  �          �   �  ,                      <�          ��  ��      ��                  ?	  m	  ��              �                    O   ����    e�          O   ����    R�          O   ����    ��      �     
                                     (                         � ߱        h�  $ S	  آ  ���                           O   k	  ��  ��  h               ԣ          ģ  ̣    ��                                             ��                            ����                                8;      $�      ��     V     ܣ                       أ  �                     8�    �	  ��  �      t      4   ����t                 �                      ��                  �	  
                  x^                       �	  ��  4�  �   �	  �      H�  �   �	  H      \�  �   �	  �      p�  �   �	  @      ��  �   �	  �      ��  �   �	  8      ��  �   �	  �      ��  �   �	  (      ԥ  �   �	  �      �  �   �	         ��  �   �	  �      �  �   �	        $�  �   �	  �          �   �	        ��    D
  T�  Ц      x      4   ����x                �                      ��                  E
  �
                  6d                       E
  d�  ��  �   G
  �      �  �   H
  T      �  �   I
  �      0�  �   J
  D      D�  �   K
  �      X�  �   L
  �      l�  �   N
  p      ��  �   O
  �      ��  �   P
  X      ��  �   Q
  �      ��  �   R
  �      Ч  �   S
  D       �  �   T
  �       ��  �   U
  �       �  �   V
  x!       �  �   W
  �!      4�  �   X
  h"      H�  �   Y
  �"      \�  �   Z
  `#      p�  �   [
  �#      ��  �   \
  X$      ��  �   ]
  �$      ��  �   ^
  �$      ��  �   _
  L%      Ԩ  �   `
  �%      �  �   a
  <&      ��  �   b
  �&      �  �   c
  4'      $�  �   d
  �'      8�  �   e
  ,(      L�  �   f
  h(      `�  �   h
  �(      t�  �   i
  X)      ��  �   j
  �)      ��  �   k
  *      ��  �   l
  �*      ĩ  �   m
  �*      ة  �   n
  l+      �  �   o
  �+       �  �   p
  \,      �  �   q
  �,      (�  �   r
  L-      <�  �   s
  �-      P�  �   t
  <.      d�  �   u
  �.      x�  �   v
  4/      ��  �   w
  �/          �   x
  $0      d�    �
  ��  8�      T0      4   ����T0                H�                      ��                  �
  �                  ��                       �
  ̪  \�  �   �
  �0      p�  �   �
  (1      ��  �   �
  �1      ��  �   �
  2      ��  �   �
  �2      ��  �   �
  3      ԫ  �   �
  |3      �  �   �
  �3      ��  �   �
  t4      �  �   �
  �4      $�  �      l5      8�  �     �5      L�  �     d6      `�  �     �6      t�  �     L7      ��  �     �7      ��  �     <8      ��  �     �8      Ĭ  �     ,9      ج  �   	  �9      �  �   
  :       �  �     X:      �  �     �:      (�  �     H;      <�  �     �;      P�  �     8<          �     �<      x�    �  ��  ��      =      4   ����=  	              �                      ��             	     �  0                  4a                       �  ��   �  �   �  |=      4�  �   �  �=      H�  �   �  t>      \�  �   �  �>      p�  �   �  l?      ��  �   �  �?      ��  �   �  \@      ��  �   �  �@      ��  �   �  TA      Ԯ  �   �  �A      �  �   �  DB      ��  �   �  �B      �  �   �  <C      $�  �   �  �C      8�  �   �  ,D      L�  �   �  �D      `�  �   �  $E      t�  �   �  �E      ��  �   �  F      ��  �   �  �F      ��  �   �  G      į  �   �  �G      د  �   �  �G      �  �   �  8H       �  �   �  �H      �  �   �  0I      (�  �   �  �I      <�  �   �   J          �   �  �J      getRowObjUpdStatic  deleteRecordStatic  �    �  ��  ��      K      4   ����K      /   �  а     �                          3   ����K             �                      3   ����<K  Զ    �  ,�  ��  �  XK      4   ����XK  
              ��                      ��             
     �  R                  �*                       �  <�  ̱  �   �  �K      $�  $  �  ��  ���                       �K     
                    � ߱        8�  �   �  L      ��  $   �  d�  ���                       ,L  @         L              � ߱        L�  $  �  ��  ���                       �L       	       	           � ߱        �M     
                N                     \O  @        
 O              � ߱        ܳ  V     �  ���                        hO       	       	       �O       
       
       �O       	       	           � ߱        l�  $  !  x�  ���                       �P     
                Q                     dR  @        
 $R              � ߱            V   3  �  ���                                      ̵                      ��                  T  �                  0,                       T  ��  pR     
                �R                     <T  @        
 �S          �T  @        
 dT          U  @        
 �T          dU  @        
 $U              � ߱            V   i  �  ���                        adm-clone-props �  ��              �     W     `                          \  �                     start-super-proc    �  d�  �           �     X                                  �                     l�    	  �   �      �X      4   �����X      /   
  ,�     <�                          3   ���� Y            \�                      3   ���� Y  ķ  $  $  ��  ���                       @Y                         � ߱        ��    4  �  \�  ��  \Y      4   ����\Y                и                      ��                  5  9                  ��                       5  �  pY                     �Y                     �Y                         � ߱            $  6  l�  ���                             :  �  T�      �Y      4   �����Y  �Y                         � ߱            $  ;  (�  ���                       |�    B  ��  ��  �  �Y      4   �����Y      $  C  ع  ���                       Z                         � ߱            �   `  Z      XZ     
                �Z                     $\  @        
 �[              � ߱        ��  V   t  �  ���                        ��  �   �  0\      ��    &  غ  �      p\      4   ����p\      /   '  �     $�                          3   �����\            D�                      3   �����\  �\     
                <]                     �^  @        
 L^              � ߱        H�  V   3  T�  ���                        �^     
                T_                     �`  @        
 d`              � ߱        t�  V   W  �  ���                        ��    �  ��  �      �`      4   �����`                �                      ��                  �  �                  lZ                       �  ��  ��  /   �  H�     X�                          3   �����`            x�                      3   �����`      /   �  ��     Ľ                          3   ����a            �                      3   ����$a   �  /  D   �         Xa                      3   ����@a  initProps   x�  0�                   Y     �             �          �  �  	                                   t�          �  �      ��                �  �  4�              X�                    O   ����    e�          O   ����    R�          O   ����    ��      �                      L�          ��  p   �  ,|  ��      �  ��  �     8|                �                      ��                  �  �                  Ę                       �  ��  4�  :  �                 $  �  `�  ���                       L|                         � ߱        �  �     d|                                        ��                  �  �                  �'                       �  ��  ��  ��     x|                                        ��                  �                    T(                       �  (�  0�   �     �|                                        ��                  	  %                  $)                       	  ��  ��  ��     �|                                        ��                  &  B                  �)                       &  @�  H�  8�     �|                                        ��                  C  _                  ��                       C  ��  ��  ��     �|                                        ��                  `  |                  |�                       `  X�  `�  P�     �|                                        ��                  }  �                  L�                       }  ��  ��  ��     �|  	                                      ��             	     �  �                  �                       �  p�  x�  h�     }  
                                      ��             
     �  �                  �                       �  ��  �  ��     }                                        ��                  �  �                  �                       �  ��  ��  ��     ,}                                        ��                  �                    �	                       �  �  �  �     @}                                        ��                    *                  �
                         ��  ��  ��     T}                                        ��                  +  G                  h                       +  ,�  4�  $�     h}                                        ��                  H  d                  l�                       H  ��  ��  ��     |}                                        ��                  e  �                  ��                       e  D�  L�  <�     �}                                        ��                  �  �                  ��                       �  ��      ��     �}                                        ��                  �  �                  ��                       �  \�      O   �  ��  ��  �}               \�          D�  P�   , $�                                                       �     ��                            ����                            <�  d�  X�  ��      ��     Z     d�                      � `�  �                     ��    �  �  ��      �}      4   �����}                ��                      ��                  �  �                  �zd                       �  ,�  �  /   �  ��     ��                          3   �����}            �                      3   �����}  ��  /   �  @�     P�                          3   ����~            p�                      3   ����,~  ��  /   �  ��     ��                          3   ����H~            ��                      3   ����h~      /   �  �     (�                          3   �����~            H�                      3   �����~  �~     
                D                     ��  @        
 T�              � ߱        ��  V   L  X�  ���                        ��  $  f  �  ���                       ��                         � ߱        ̀     
                H�                     ��  @        
 X�              � ߱        ��  V   p  @�  ���                        ��  $  �  ��  ���                       ��     
                    � ߱        ��     
                4�                     ��  @        
 D�              � ߱        ��  V   �  (�  ���                        t�  $  �  ��  ���                       ��     
                    � ߱        ��     
                 �                     p�  @        
 0�              � ߱        ��  V   �  �  ���                        \�  $  �  ��  ���                       ��                         � ߱        ��     
                ,�                     |�  @        
 <�              � ߱        ��  V   �  ��  ���                        ��  �   �  ��      X�  $  �  ��  ���                       ��     
                    � ߱        Ȉ     
                D�                     ��  @        
 T�              � ߱        ��  V     ��  ���                        ��  $    ��  ���                       ��     
                    � ߱        ��  �   6  ��      H�  $  @  �  ���                       �     
                    � ߱        \�  �   Z  �      ��  $  |  ��  ���                       H�                         � ߱              �  ��  ��      d�      4   ����d�      /   �  �     �                          3   ������  L�     
   <�                      3   ������  |�        l�                      3   ������  ��        ��                      3   ������            ��                      3   ����܋  pushRowObjUpdTable  ��  ��  �                   [      �                               �                     pushTableAndValidate    ��  L�  �           |     \     �                          �  �                     remoteCommit    d�  ��  �           p     ]     �                          �  �                     serverCommit    ��  ,�  �           l     ^     �                          �  
                                      L�          �  �      ��                  �  �  4�              H�                    O   ����    e�          O   ����    R�          O   ����    ��          O   �  ��  ��  �    ��                            ����                            <�  P�      ��              _      d�                      
�                           disable_UI  ��   �                      `      �                               *   
                    �  �    ����  �       ��          ��  8   ����   ��  8   ����   ��  8   ����   ��  8   ����       8   ����       8   ����        �  �      viewObject  ,   ��   �  ,�      toggleData  ,INPUT plEnabled LOGICAL    �  X�  p�      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  H�  ��  ��      returnFocus ,INPUT hTarget HANDLE   ��  ��  ��      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    ��  8�  D�      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE (�  ��  ��      removeAllLinks  ,   ��  ��  ��      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE ��  $�  8�      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    �  ��  ��      hideObject  ,   ��  ��  ��      exitObject  ,   ��  ��  �      editInstanceProperties  ,   ��  �  ,�      displayLinks    ,   �  @�  P�      createControls  ,   0�  d�  t�      changeCursor    ,INPUT pcCursor CHARACTER   T�  ��  ��      applyEntry  ,INPUT pcField CHARACTER    ��  ��  ��      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER ��  @�  L�      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER 0�  ��  ��      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��   �  �      unbindServer    ,INPUT pcMode CHARACTER ��  8�  H�      runServerObject ,INPUT phAppService HANDLE  (�  t�  ��      disconnectObject    ,   d�  ��  ��      destroyObject   ,   ��  ��  ��      bindServer  ,   ��  ��  ��      transferDBRow   ,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER  ��  4�  @�      startFilter ,   $�  T�  d�      releaseDBRow    ,   D�  x�  ��      refetchDBRow    ,INPUT phRowObjUpd HANDLE   h�  ��  ��      filterContainerHandler  ,INPUT phFilterContainer HANDLE ��  ��  �      fetchDBRowForUpdate ,   ��  $�  4�      confirmContinue ,INPUT-OUTPUT pioCancel LOGICAL �  d�  t�      compareDBRow    ,   T�  ��  ��      bufferCopyDBToRO    ,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER   x�  �  �      assignDBRow ,INPUT phRowObjUpd HANDLE    �  H�  T�      updateState ,INPUT pcState CHARACTER    8�  ��  ��      updateQueryPosition ,   p�  ��  ��      updateAddQueryWhere ,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER    ��   �  �      undoTransaction ,   ��  $�  4�      transferToExcel ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER  �  ��  ��      synchronizeProperties   ,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER   ��  ,�  @�      submitValidation    ,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER   �  ��  ��      submitForeignKey    ,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER  |�  �  $�      submitCommit    ,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL  �  h�  |�      startServerObject   ,   X�  ��  ��      setPropertyList ,INPUT pcProperties CHARACTER   ��  ��  ��      serverFetchRowObjUpdTable   ,OUTPUT TABLE-HANDLE phRowObjUpd    ��   �  0�      serverSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject    �  ��   �      saveContextAndDestroy   ,OUTPUT pcContext CHARACTER ��  ,�  <�      rowObjectState  ,INPUT pcState CHARACTER    �  h�  x�      retrieveFilter  ,   X�  ��  ��      restartServerObject ,   |�  ��  ��      remoteSendRows  ,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject,OUTPUT pocMessages CHARACTER   ��  ��  ��      refreshRow  ,   ��  ��  ��      printToCrystal  ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER  ��  P�  `�      isUpdatePending ,INPUT-OUTPUT plUpdate LOGICAL  @�  ��  ��      initializeServerObject  ,   ��  ��  ��      initializeObject    ,   ��  ��  ��      home    ,   ��   �  �      genContextList  ,OUTPUT pcContext CHARACTER ��  <�  H�      fetchPrev   ,   ,�  \�  h�      fetchNext   ,   L�  |�  ��      fetchLast   ,   l�  ��  ��      fetchFirst  ,   ��  ��  ��      fetchBatch  ,INPUT plForwards LOGICAL   ��  ��  �      endClientDataRequest    ,   ��   �  4�      destroyServerObject ,   �  H�  X�      describeSchema  ,INPUT pcSdoName CHARACTER,OUTPUT TABLE-HANDLE hTtSchema    8�  ��  ��      dataAvailable   ,INPUT pcRelative CHARACTER ��  ��  ��      copyColumns ,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE ��  4�  H�      commitTransaction   ,   $�  \�  l�      clientSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER    L�  �  �      batchServices   ,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER        � 
"     
 a%     adecomm/as-utils.w 
"   
   �    }        �
"     
    �     }        �� ,   J   %               � 
" 	   
 �%              h �P  \         (          
�                          
�            � �   �
" 	   
 
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        �    7%               
"   
 s�               1� �  
 s� �   �%               o%   o           � �    s
"   
 s�           �    1� �   s� �   �%               o%   o           � �   s
"   
 s�           �    1� �  
 s� �   �%               o%   o           � �   s
"   
 s�           l    1� �   s� �   �%               o%   o           � �    s
"   
 s�           �    1� �   s� �   �%               o%   o           � �   s
"   
 s�           T    1� 
   s�    �%               o%   o           %               
"   
 ��          �    1�    �� .     
"   
 s�               1� 5   s� �   �%               o%   o           � H  s
"   
 s�           �    1� J   s� �   �%               o%   o           � Y  S s
"   
 s�           �    1� �   s�    �%               o%   o           %               
"   
 s�           p    1� �   s�    �%               o%   o           %               
"   
 s�           �    1� �   s�    �%               o%   o           %              
"   
 ��          h    1� �   ��      
"   
 s�           �    1� �  
 s�    �%               o%   o           %               
"   
 s�                1� �   s� �   �%               o%   o           � �    s
"   
 ��          �    1� �   �� .     
"   
 s�           �    1�    s� �   �%               o%   o           � $  t s
"   
 ��          D	    1� �  
 �� .     
"   
 s�           �	    1� �   s� �   �%               o%   o           � �  � s
"   
 s�           �	    1� B   s� �   �%               o%   o           � �    s
"   
 s�           h
    1� Y  
 s� d   �%               o%   o           %               
"   
 �           �
    1� h   �    �%               o%   o           %              
"   
 �           `    1� p   � �   �%               o%   o           � �    
"   
 �           �    1� �   � �   �%               o%   o           o%   o           
"   
 �           P    1� �  
 � �   �%               o%   o           � �    
"   
 �           �    1� �   � �  	 �%               o%   o           � �  / 
"   
 ��          8    1� �   �� �  	   
"   
 �           t    1� �   � �  	 �o%   o           o%   o           � �    
"   
 ��          �    1�    �� �  	   
"   
 �           $    1�    � �  	 �o%   o           o%   o           � �    
"   
 ��          �    1� +   ��      
"   
 ��          �    1� 9   �� �  	   
"   
 ��              1� F   �� �  	   
"   
 ��          L    1� S   �� �  	   
"   
 �           �    1� a   �    �o%   o           o%   o           %              
"   
 ��              1� r   �� �  	   
"   
 ��          @    1� �  
 �� �     
"   
 ��          |    1� �   �� �  	   
"   
 ��          �    1� �   �� �  	   
"   
 ��          �    1� �   �� �  	   
"   
 ��          0    1� �   �� �  	   
"   
 ��          l    1� �  	 �� �  	   
"   
 ��          �    1� �   �� �  	   
"   
 ��          �    1� �   �� �  	   
"   
 �                1�    � �   �%               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 
"   
   
"   
 �(�  L ( l       �        �    ��    � P   �        �    �@    
� @  , 
�            �� "     p�               �L
�    %              � 8          � $         � )          
�    � C     
"   
 �� @  , 
�           �� �  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 �           �    1� F  
 � �   �%               o%   o           � �    
"   
 �           <    1� Q  
 � �   �%               o%   o           o%   o           
"   
 �           �    1� \   � .   �%               o%   o           o%   o           
"   
 �           4    1� e   �    �%               o%   o           %               
"   
 �           �    1� t   �    �%               o%   o           %               
"   
 a�           ,    1� �   a� �   �%               o%   o           � �    
"   
 �           �    1� �   �    �%               o%   o           %              
"   
 �               1� �   �    �%               o%   o           o%   o           
"   
 �           �    1� �   � �   �%               o%   o           o%   o           
"   
 �               1� �  	 � �   �%               o%   o           � �    
"   
 �           �    1� �   � �   �%               o%   o           o%   o           
"   
 �               1� �   � �   �%               o%   o           o%   o           
"   
 �           �    1� �   �    �%               o%   o           %               
"   
 �           �    1� �   �    �%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 �           �    1� �  
 �    �%               o%   o           %              
"   
 �           H    1�    � �   �%               o%   o           o%   o           
"   
 �           �    1�    � �   �%               o%   o           � �    
"   
 �           8    1� "   � �   �%               o%   o           o%   o           
"   
 ��          �    1� .   �� .     
"   
 �           �    1� ;   � �   �%               o%   o           � N  ! 
"   
 �           d    1� p   � �   �%               o%   o           � �    
"   
 �           �    1� }   � �   �%               o%   o           � �   
"   
 ��          L    1� �   �� �     
"   
 ��          �    1� �   �� .     
"   
 �           �    1� �   � �   �%               o%   o           � �    
"   
 ��          8     1� �  
 �� .     
"   
 a�           t     1� �   a�    �%               o%   o           o%   o           
"   
 �           �     1� �   �    �%               o%   o           %               
"   
 �           l!    1� �   �    �%               o%   o           %               
"   
 �           �!    1� 	   � �   �%               o%   o           � �    
"   
 �           \"    1�    � �   �%               o%   o           o%   o           
"   
 �           �"    1� %   �    �%               o%   o           %              
"   
 �           T#    1� 6   �    �%               o%   o           %               
"   
 a�           �#    1� C   a�    �%               o%   o           %               
"   
 ��          L$    1� S   �� .     
"   
 ��          �$    1� `   �� �     
"   
 �           �$    1� m   � d   �%               o%   o           o%   o           
"   
 �           @%    1� y   � �   �%               o%   o           � �    
"   
 �           �%    1� �   � �   �%               o%   o           o%   o           
"   
 �           0&    1� �   �    �o%   o           o%   o           o%   o           
"   
 �           �&    1� �   � �  	 �%               o%   o           o%   o           
"   
 �           ('    1� �   � �   �%               o%   o           o%   o           
"   
 �           �'    1� �  
 � d   �%               o%   o           o%   o           
"   
 ��           (    1� �   �� �     
"   
 �           \(    1� �   � �   �%               o%   o           � �  4 
"   
 �           �(    1� 0  
 �    �%               o%   o           %              
"   
 ��          L)    1� ;   �� .     
"   
 �           �)    1� L   � �   �%               o%   o           � �    a
"   
 �           �)    1� Z   �    �%               o%   o           %              
"   
 �           x*    1� i   � �   �%               o%   o           � �    
"   
 �           �*    1� v   � �   �%               o%   o           � �    
"   
 �           `+    1� �   � �   �%               o%   o           � �    
"   
 �           �+    1� �   �    �%               o%   o           %               
"   
 �           P,    1� �  	 � .   �%               o%   o           o%   o           
"   
 a�           �,    1� �   a� �   �%               o%   o           � �  	 
"   
 �           @-    1� �   � d   �%               o%   o           %       �       
"   
 �           �-    1� �   � �   �%               o%   o           � �    
"   
 �           0.    1� �   �    �o%   o           o%   o           %              
"   
 �           �.    1� �   �    �%               o%   o           %               
"   
 �           (/    1� �   � �   �%               o%   o           o%   o           
"   
 �           �/    1�    � �  	 �%               o%   o           � �    
"   
 ��          0    1�     �� �  	   P �L 
�H T   %              �     }        �GG %              
"   
 a�           �0    1� -  
 a� �   �%               o%   o           � �    a
"   
 �           1    1� 8   �    �%               o%   o           %               
"   
 �           �1    1� E  	 � �   �%               o%   o           � �    
"   
 �           2    1� O   � �   �%               o%   o           � �    
"   
 �           �2    1� ]   �    �%               o%   o           %               
"   
 �           �2    1� m   � �   �%               o%   o           � �    
"   
 �           p3    1� �   � �   �%               o%   o           o%   o           
"   
 �           �3    1� �   � �   �%               o%   o           o%   o           
"   
 �           h4    1� �   �    �%               o%   o           o%   o           
"   
 a�           �4    1� �   a�    �%               o%   o           o%   o           
"   
 �           `5    1� �   �    �%               o%   o           o%   o           
"   
 �           �5    1� �   � �   �%               o%   o           o%   o           
"   
 �           X6    1� �  	 � �  	 �%               o%   o           � �    
"   
 �           �6    1� �  
 � �  	 �%               o%   o           � �    
"   
 �           @7    1� �   � �   �%               o%   o           � �    
"   
 �           �7    1� �   � �   �%               o%   o           o%   o           
"   
 �           08    1�    � �   �%               o%   o           o%   o           
"   
 �           �8    1�    � �   �%               o%   o           � �    
"   
 �            9    1� '   � �   �%               o%   o           � �    
"   
 �           �9    1� 6   � �  	 �%               o%   o           o%   o           
"   
 ��          :    1� H   �� .     
"   
 �           L:    1� T   � �   �%               o%   o           � �    
"   
 �           �:    1� b   � �   �%               o%   o           o%   o           
"   
 a�           <;    1� u   a�    �%               o%   o           o%   o           
"   
 �           �;    1� �  
 � �   �%               o%   o           � �    
"   
 �           ,<    1� �   � �   �%               o%   o           � �    
"   
 �           �<    1� �   �    �%               o%   o           %               P �L 
�H T   %              �     }        �GG %              
"   
 �           p=    1� �  	 � .   �%               o%   o           o%   o           
"   
 �           �=    1� �   � .   �%               o%   o           o%   o           
"   
 �           h>    1� �   � .   �%               o%   o           o%   o           
"   
 a�           �>    1� �   a�    �%               o%   o           %              
"   
 �           `?    1� �   � �   �%               o%   o           �   M a
"   
 �           �?    1� ^   �    �%               o%   o           %              
"   
 �           P@    1� o   �    �%               o%   o           %               
"   
 �           �@    1� �   �    �%               o%   o           %               
"   
 �           HA    1� �   � �  	 �%               o%   o           � �   
"   
 �           �A    1� �   �    �%               o%   o           %               
"   
 �           8B    1� �   � �  	 �%               o%   o           o%   o           
"   
 �           �B    1� �   �    �o%   o           o%   o           %              
"   
 a�           0C    1� �   a� �  	 �o%   o           o%   o           � �    a
"   
 �           �C    1� �   � .   �o%   o           o%   o           o%   o           
"   
 �            D    1�    � .   �o%   o           o%   o           o%   o           
"   
 �           �D    1�    � �  	 �o%   o           o%   o           o%   o           
"   
 �           E    1� ,   � .   �o%   o           o%   o           o%   o           
"   
 �           �E    1� ;   � �  	 �o%   o           o%   o           � I   
"   
 �           F    1� K   � �  	 �o%   o           o%   o           � Z   
"   
 �           |F    1� f   �    �%               o%   o           %               
"   
 �           �F    1� z   �    �%               o%   o           %               
"   
 ��          tG    1� �   �� �  	   
"   
 �           �G    1� �   �    �%               o%   o           %               
"   
 �           ,H    1� �   � �   �%               o%   o           o%   o           
"   
 �           �H    1� �   � �   �%               o%   o           o%   o           
"   
 �           $I    1� �   �    �%               o%   o           o%   o           
"   
 �           �I    1� �   � �   �%               o%   o           � �    
"   
 �           J    1� �   �    �%               o%   o           %               
"   
 �           �J    1�   	 �    �%               o%   o           %                "    �%     start-super-proc ��%     adm2/smart.p �P �L 
�H T   %              �     }        �GG %              
"   
   �       �K    6�      
"   
   
�        �K    8
"   
   �        �K    ��     }        �G 4              
"   
 ߱G %              G %              %�   AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets  
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �M    ��    � P   �        �M    �@    
� @  , 
�       �M    �� "   �p�               �L
�    %              � 8       N    � $         � )          
�    � C   �
"   
 �p� @  , 
�       O    �� 5   �p�               �L"  	  , �   � C   � E   ��     }        �A      |    "  	    � C   %              (<   \ (    |    �     }        �A� G   �A"  
      "  	  �"  
    < "  	  �"  
  (    |    �     }        �A� G   �A"  
  
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �P    ��    � P   �        �P    �@    
� @  , 
�       �P    �� "   �p�               �L
�    %              � 8      Q    � $         � )          
�    � C   �
"   
 �p� @  , 
�       R    �� �  
 �p�               �L"  	  , 
�H T   %              �     }        �GG %              
"   
   
"   
 
"   
   
"   
   (�  L ( l       �        �R    ��    � P   �        �R    �@    
� @  , 
�       �R    �� "     p�               �L
�    %              � 8      �R    � $         � )          
�    � C     
"   
 �p� @  , 
�       �S    �� �  
 �p�               �L%     SmartDataObject 
"   
   p� @  , 
�       XT    �� �     p�               �L%               
"   
  p� @  , 
�       �T    ��     p�               �L%               
"   
  p� @  , 
�       U    �� �    p�               �L(        � �      � �      � �      �     }        �A
�H T   %              �     }        �GG %              
"   
  (   � 
"   
 �    �        �U    ��    �
"   
   � 8      DV    � $         � )          
�    � C   �
"   
   �        �V    �
"   
   �       �V    /
"   
   
"   
   �       �V    6�      
"   
   
�        W    8
"   
   �        4W    �
"   
   �       TW    �
"   
   p�    � p   
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 �    �        X    �A"    �A
"   
   
�        dX    �@ � 
"   
 "      �       }        �
"   
 �%              %                "    �%     start-super-proc ��%     adm2/appserver.p ��    � �     
�    �     }        �%               %      Server  - �     }        �    "    � �    �%                   "    � �    �%      NONE    p�,  8         $     "            �    �
�    
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �Z    ��    � P   �        �Z    �@    
� @  , 
�       �Z    �� "   �p�               �L
�    %              � 8      �Z    � $         � )          
�    � C   �
"   
 �p� @  , 
�       �[    �� �   �p�               �L"    , p�,  8         $     "            �    �
�     "    �%     start-super-proc ��%     adm2/dataquery.p �a
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 �(�  L ( l       �        ]    ��    � P   �        ]    �@    
� @  , 
�       $]    �� "   �p�               �L
�    %              � 8      0]    � $         � )   �     
�    � C   �
"   
 �p� @  , 
�       @^    �� �   �p�               �L%H > 8   dataAvailable,confirmContinue,isUpdatePending,buildDataRequest  
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 �(�  L ( l       �        $_    ��    � P   �        0_    �@    
� @  , 
�       <_    �� "   �p�               �L
�    %              � 8      H_    � $         � )   �     
�    � C   �
"   
 �p� @  , 
�       X`    �� 
   �p�               �L%               "    �%     start-super-proc ��%     adm2/query.p �%     start-super-proc ��%     adm2/queryext.p % 	    initProps �
�    %8 , (   FOR EACH FacCPedi NO-LOCK INDEXED-REPOSITION ��   � �     � �     �       
�     	         �G
"   
 �        �a    �G
"   
   
"   
    x    (0 4      �        �a    �G%                   �        b    �GG %              � m    �� n         %              %                   "      %              
"   
       "      �        �b    �
"   
   �        ,c    �
"   
   
�       Lc    �"       \      H   "    �((       "      %              � �      � �   �     
"   
   
"   
 � \      H   "      ((       "      %              � �     � �   �        �c    �%                   %              %                   "  (    %                  "  (        
"   
 �
"   
 0 T       m � "  (  a�        �d    �A @   "      $         � "  (  � G   ��        e    �� "  (    
"   
 � \ H     H   "      ((       "    �%              � �    �� �     (        "  !  �� �    �        �e    �"  !  �
�H T   %              �     }        �GG %              
"   
 
"   
 
"   
   
"   
   (�  L ( l       �        �f    ��    � P   �        �f    �@    
� @  , 
�       �f    �� "     p�               �L
�    %              � 8      �f    � $         � )          
�    � C     
"   
 �p� @  , 
�       �g    �� �   �p�               �L%               
"   
   p� @  , 
�       Hh    ��      p�               �L"    , �,  8         $     "    �L        � u  
  
�    
�H T   %              �     }        �GG %              
"   
 $ 
"   
   
"   
 �
"   
 �(�  L ( l       �        ,i    ��    � P   �        8i    �@    
� @  , 
�       Di    �� "   �p�               �L
�    %              � 8      Pi    � $         � )   �     
�    � C     
"   
 �p� @  , 
�       `j    �� H   �p�               �L
"   
 , 
"   
   p� @  , 
�       �j    �� '     p�               �L"    , 
"   
  p� @  , 
�       k    �� �    p�               �L"    ,     "    � �    �%L C <   OPEN QUERY Query-Main FOR EACH FacCPedi NO-LOCK INDEXED-REPOSITION.     "    CP� �   K ((        "    PO%                   "    �� �     "    � (   "           "    �%              @ �,  8         $     "    �        � �    
�    p�,  8         $     � �           � �   �
�    %               �    "      � �         %              %                   "      %                  "      "      "     T(        "    %              "    � �   �"      �       "    ��    "    � G   �� �      � G   ��    "     � G    S    "      "    �    "    %                � @    �     t T     P   4       �"      (0       4       "      � �      � �    �� �   T ,  %              T   "    "    �� �     � G   �� �   T    �    "    � G   �"      � G   �"      %                   %              %                   "      %                  "      �     "      �     "       \      H   "      ((       "    �%              � �    �� �     4       "      
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �q    ��    � P   �        �q    �@    
� @  , 
�       �q    �� "   �p�               �L
�    %              � 8      �q    � $         � )          
�    � C   �
"   
 �p� @  , 
�       �r    �� -  
 �p�               �L"    ,       "  
  �    � �  � � �   �      "  	    �    � �  � �� �   �   � �     � �     � �  � ��   � �     � �   �� �  � �   � �     � �     � �  �   
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 �(�  L ( l       �        Ht    ��    � P   �        Tt    �@    
� @  , 
�       `t    �� "   �p�               �L
�    %              � 8      lt    � $         � )          
�    � C     
"   
 �p� @  , 
�       |u    �� �   �p�               �L"    , 
"   
   p� @  , 
�       �u    �� �     p�               �L"    , 
"   
  p� @  , 
�       ,v    �� m    p�               �L"    ,     %              %                   "      %                  "      �     "      �     "      4 (        "  
    �    � �  �   � �         "  	  �     "    T    "      "      @ A,    �   � �   �� �     "    �"       T      @   "    �(        "      � �    �� �      � �   �"         "  	   %              D H   @ A,    �   � �   �� �     "    �"    ,    S   "    �� �  � � �   �%                T      @   "    �(        "      � �    �� �      � �   �"         "  
   %                         "    �� �     "    �           "      � �   �"      
�H T   %              �     }        �GG %              
"   
 
"   
   
"   
 
"   
 �(�  L ( l       �        Hz    ��    � P   �        Tz    �@    
� @  , 
�       `z    �� "   p�               �L
�    %              � 8      lz    � $         � )   �     
�    � C   �
"   
 �p� @  , 
�       |{    �� �   �p�               �L"    , 
"   
   p� @  , 
�       �{    �� m     p�               �L"    , "      %               �     }        �%              %              %              %              %              %              %              %       	       %       
       %              %              %              %              %              %              %              %              "       "    �%     start-super-proc �%     adm2/data.p %     start-super-proc �%     adm2/dataext.p %     start-super-proc �%     adm2/dataextcols.p %     start-super-proc �%     adm2/dataextapi.p 
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 �(�  L ( l       �            ��    � P   �             �@    
� @  , 
�       ,    �� "   �p�               �L
�    %              � 8      8    � $         � )   �     
�    � C   �
"   
 �p� @  , 
�       H�    �� �   �p�               �L%               %      "vta2/dcotcredito.i" �
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �    ��    � P   �        $�    �@    
� @  , 
�       0�    �� "   �p�               �L
�    %              � 8      <�    � $         � )          
�    � C   �
"   
 �p� @  , 
�       L�    �� �   �p�               �L"    , 
�     	        �G
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �    ��    � P   �        �    �@    
� @  , 
�       �    �� "   �p�               �L
�    %              � 8      (�    � $         � )          
�    � C   �
"   
 �p� @  , 
�       8�    �� �  
 �p�               �L
"   
 , 
�     
        �G
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        ��    ��    � P   �        ��    �@    
� @  , 
�       �    �� "   �p�               �L
�    %              � 8      �    � $         � )          
�    � C   �
"   
 �p� @  , 
�       $�    �� �  	 �p�               �L
"   
 , 
"   
 �     � >  	   �        |�    �
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        ��    ��    � P   �        �    �@    
� @  , 
�       �    �� "   �p�               �L
�    %              � 8       �    � $         � )          
�    � C   �
"   
 �p� @  , 
�       0�    �� 	   �p�               �L"    , 
"   
   �       ��    �"      
�     
        �G
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �    ��    � P   �         �    �@    
� @  , 
�       ,�    �� "   �p�               �L
�    %              � 8      8�    � $         � )          
�    � C   �
"   
 �p� @  , 
�       H�    �� �  	 �p�               �L
"   
 , 
�             �Gp�,  8         $     
"   
         � H   �
�    
�             �Gp�,  8         $     
"   
         � Z   �
�    �    � l     
�        "    � �    �%     modifyListProperty 
�    %      REMOVE  %     SupportedLinks %     Update-Target  %     bufferValidate  
�    "      �  %      setContextAndInitialize 
�    "      %     bufferCommit    
�    "      "      �    � �     
�    %               %     bufferCommit    
�    "      "      
�     
        �G�     }        �
�                    �           �   l       ��                 a  �  �               L'                    O   ����    e�          O   ����    R�          O   ����    ��        $  p  �   ���                       �U     
                    � ߱              q  (  �      V      4   ����V                �                      ��                  r  �                  ��                       r  8  �  �  s  PV            u  �  `      �V      4   �����V                p                      ��                  v  �                  \�                       v  �  �  o   w      ,                                 �  �   x  �V      �  �   y  �V      $  $  z  �  ���                        W     
                    � ߱        8  �   {  @W      L  �   |  `W      `  �     �W          $   �  �  ���                       �W  @         �W              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �  �  �               ��                    O   ����    e�          O   ����    R�          O   ����    ��      �                      �          �  $  �    ���                       X     
                    � ߱                  �  �                      ��                   �  �                  ܝ                     �  4      4   ����$X      $  �  �  ���                       pX     
                    � ߱        �    �  4  D      �X      4   �����X      /  �  p                               3   �����X  �  �   �  �X          O   �  ��  ��  �X                               , �                          
                               �      ��                            ����                                            �           �   l       ��            	     P  �  �               �[                    O   ����    e�          O   ����    R�          O   ����    ��        $  p  �   ���                       `a                         � ߱        �  $  q  8  ���                       �a                         � ߱        �a     
                b  @         �a              � ߱        D  $   �  d  ���                         T      �  x                      ��        0         �  �                  ��      8c     L     �  �      $  �  �  ���                       �b                         � ߱          $  �  �  ���                       �b                         � ߱            4   ����c  Xc     
                xc                     �c                         � ߱          $  �    ���                                                            ��                  �  �                  ��                �     �  �  �  $  �  L  ���                       td       !       !           � ߱          �      L  �                      ��        0         �  �                  �     ( �d            �  x      $  �     ���                       �d       (       (           � ߱        �  $  �  x  ���                       �d       (       (           � ߱            4   �����d        �  �  L      e      4   ����e                \                      ��                  �  �                  t`                       �  �  �  $  �  �  ���                       �e       !       !           � ߱            O   �  �� ��          $  �  �  ���                       �e                         � ߱        hf     
                �f                     4h  @        
 �g          �h  @        
 Th          �h                     �h     
                \i                     �j  @        
 lj          k  @        
 �j          \k  @        
 k              � ߱        x  V   �  $  ���                        P	    �  �  $	      hk      4   ����hk  �k                     �k                     �k                     Tl                         � ߱            $  �  �  ���                       �	    �  l	  |	      �l      4   �����l      �   �  �l      �	  $  �  �	  ���                       m                         � ߱        �
  $  �  
  ���                       (m                         � ߱          �
                              ��        0         �  �                  <@      �m     �     �  @
      $  �  �
  ���                       Hm                         � ߱        l  $  �  @  ���                       xm                         � ߱            4   �����m  �m                     n                      n                     pn                     �n                         � ߱        D  $  �  |  ���                             �  `  p      �n      4   �����n      $  �  �  ���                       �n          p             � ߱        �  $  �  �  ���                       p                         � ߱          �      �  \                      ��        0         �  �                  ��      �p          �         $  �  �  ���                       $p                         � ߱        L  $  �     ���                       Tp                         � ߱            4   ����|p      $  �  �  ���                       �p                         � ߱        8q     
                �q                     s  @        
 �r              � ߱        �  V   �  �  ���                        s       
       
       Ds       	       	       xs                     �s                         � ߱        �  $    D  ���                       �  $  �    ���                       �s                         � ߱        �s     
                xt                     �u  @        
 �u           v  @        
 �u          xv  @        
 8v              � ߱        |  V   �  H  ���                          �      �  \                      ��        0    	     .  C                  |�      w     4     .        $  .  �  ���                       �v                         � ߱        <  $  .    ���                       �v                         � ߱        L  4   �����v      4   ����w  �  $  3  �  ���                       |w                         � ߱        �    5  �  L      �w      4   �����w                �                      ��                  6  :                                          6  �  �w                     Hx       	       	           � ߱            $  7  \  ���                             <  �  h      px      4   ����px  	              �                      ��             	     >  B                  ��                       >  �  y                     ly       
       
           � ߱            $  ?  x  ���                       �y                     �y                         � ߱        �  $  I  �  ���                       �y     
                xz                     �{  @        
 �{           |  @        
 �{              � ߱            V   W  `  ���                                    J `          �  �  � X@                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
             
                                                                                                                                                                                                                               ) �   �   �   �       (  8  H  X  h  x  �  �  �  �  �  �      (  8  H  X  h  x  �  �  �  �  �  �  �  �      (  8  H  8HXhx��������(8HX     ) �   �   �   �      (  8  H  X  h  x  �  �  �  �  �   �     (  8  H  X  h  x  �  �  �  �  �  �  �  �      (  8  H  8HXhx��������(8HX  �   :                  � �                     �    ��                      ��                            ����                            �                                                          �   l       ��                    %  �               �                    O   ����    e�          O   ����    R�          O   ����    ��              !                    ��                            ����                                            �           �   l       ��                  /  >  �               ��                    O   ����    e�          O   ����    R�          O   ����    ��      �       �              �                  $                  d  /  ;  $     4  �                      3   ������            T                      3   �����      O   <  ��  ��  (�               �          �  �    �                                             ��                            ����                                            H          �   l       ��                  H  s  �               $�                    O   ����    e�          O   ����    R�          O   ����    ��      �       �              �                $                  �       ,             �          �                                �  /  g  t     �  P�                      3   ����,�            �                      3   ����X�     /  i  �     �  ��                      3   ����d�  x                             3   ������      $   i  L  ���                                                   � ߱                  �  �                  3   ������      $   i  �  ���                                                   � ߱        X  $  m  ,  ���                       ��                         � ߱            O   q  ��  ��  ��               �          �  �   @ �                                                              0              0           ��                            ����                                                       �   l       ��                  }  �  �               $<                    O   ����    e�          O   ����    R�          O   ����    ��      �       $                  �                    �          �                      �              /  �  L     \  �                      3   ����Ќ  �        |  �                  3   �����      $   �  �  ���                                                   � ߱                                      3   ���� �      $   �  @  ���                                                   � ߱                     �          �  �   , �                                                                 ��                            ����                                            �           �   l       ��                    *  �               xv                    O   ����    e�          O   ����    R�          O   ����    ��            )  �   �        �      4   ���� �      �   )  4�    ��                            ����                            TXS appSrvUtils FacCPedi Pedidos al Credito d:\newsie\on_in_co\APLIC\vta2\dcotcredito.w should only be RUN PERSISTENT. ADDROW CANCELROW CANNAVIGATE CLOSEQUERY COLUMNPROPS COLVALUES COPYROW CREATEROW DELETEROW FETCHROW FETCHROWIDENT FINDROW FINDROWWHERE FIRSTROWIDS GETLASTCOMMITERRORTYPE HASFOREIGNKEYCHANGED OPENDATAQUERY OPENQUERY PREPAREQUERY ROWAVAILABLE ROWVALUES SUBMITROW UPDATEROW GETOBJECTTYPE xiRocketIndexLimit ADDQUERYWHERE ASSIGNQUERYSELECTION BUFFERCOMPAREDBTORO BUFFERWHERECLAUSE COLUMNDATATYPE COLUMNDBCOLUMN COLUMNQUERYSELECTION COLUMNTABLE COLUMNVALMSG DBCOLUMNDATANAME DBCOLUMNHANDLE EXCLUDECOLUMNS GETDATACOLUMNS GETFOREIGNVALUES GETQUERYPOSITION GETQUERYSORT GETQUERYSTRING GETQUERYWHERE GETTARGETPROCEDURE INDEXINFORMATION INSERTEXPRESSION NEWQUERYSTRING NEWQUERYVALIDATE NEWQUERYWHERE NEWWHERECLAUSE REFRESHROWIDENT REMOVEFOREIGNKEY REMOVEQUERYSELECTION RESOLVEBUFFER ROWIDWHERE ROWIDWHERECOLS SETQUERYPOSITION SETQUERYSORT SETQUERYSTRING SETQUERYWHERE WHERECLAUSEBUFFER GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDataObject ContainerType PropertyDialog adm2/support/datad.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets SupportedLinks Data-Source,Data-Target,Navigation-Target,Update-Target,Commit-Target,Filter-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext AutoCommit BLOBColumns BufferHandles CLOBColumns CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState CurrentRowid ROWID CurrentUpdateSource DataColumns DataHandle DataIsFetched DataModified DataQueryBrowsed DataQueryString FetchOnOpen FillBatchOnRepos FilterActive FilterAvailable FilterSource FilterWindow FirstRowNum ForeignFields ForeignValues IgnoreTreeViewFilter IndexInformation LargeColumns LastRowNum NavigationSource NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter OpenOnInit PrimarySDOSource PromptColumns PromptOnDelete QueryColumns QueryPosition QueryString RebuildOnRepos RowObject RowObjectState NoUpdates RowsToBatch Tables ToggleDataTargets TransferChildrenForAll UpdatableColumns UpdatableWhenNew UpdateSource AssignList AuditEnabled BaseQuery CalcFieldList CheckLastOnOpen DataColumnsByTable DBNames EntityFields FetchHasAudit FetchHasComment FetchAutoComment FirstResultRow KeyFields KeyTableId LastDBRowIdent LastResultRow NewBatchInfo NoLockReadOnlyTables PhysicalTables PositionForClient QueryHandle QueryRowIdent RequiredProperties SkipTransferDBRow TempTables UpdatableColumnsByTable UpdateFromSource RowObjUpd RowObjectTable RowObjUpdTable CheckCurrentChanged StatelessSavedProperties CheckCurrentChanged,RowObjectState,LastResultRow,FirstResultRow,QueryRowIdent DestroyStateless DisconnectAppServer ServerSubmitValidation DataFieldDefs "vta2/dcotcredito.i" QueryContainer QueryContext AsynchronousSDO DataLogicProcedure DataLogicObject DataReadHandler DataReadColumns DataReadBuffer DataDelimiter | DataReadFormat TrimNumeric IsRowObjectExternal IsRowObjUpdExternal ManualAddQueryWhere DynamicData LastCommitErrorType LastCommitErrorKeys RunDataLogicProxy SchemaLocation CacheDuration INTEGER ShareData ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService adm2/dataquery.p dataAvailable,confirmContinue,isUpdatePending,buildDataRequest adm2/query.p adm2/queryext.p cTable iTable cColumns cDataCols cUpdCols cCalcData cCalcUpd iNumData iNumUpd cBuffers cKeyFields cAssignList iAssigns iPos iEntry iCount cTables cTableAssign cDbEntry cField cKeyTable cQueryString FOR EACH FacCPedi NO-LOCK INDEXED-REPOSITION ,   hQuery cOpenQuery cDBNames cPhysicalTables cKeyTableEntityFields cKeyTableEntityValues cKeyTableEntityMnemonic cKeyTableEntityObjField cDBName cEntityFields lHasObjectField lHasAudit lHasComment lHasAutoComment iLookup iAlias  STATIC setDBNames OPEN QUERY Query-Main FOR EACH FacCPedi NO-LOCK INDEXED-REPOSITION.  FOR   PRESELECT  setOpenQuery 5 showMessage ; Atencion Cmpbnte CodCia CodCli CodDiv CodDoc CodMon CodPos CodVen DirCli FchEnt FchPed fchven FlgEst FlgIgv FmaPgo Glosa LugEnt NomCli NroPed ordcmp RucCli Sede TpoCmb usuario Query-Main INITPROPS piTableIndex lRet DELETERECORDSTATIC adm2/data.p adm2/dataext.p adm2/dataextcols.p adm2/dataextapi.p Atencion Cmpbnte CodCia CodCli CodDiv CodDoc CodMon CodPos CodVen DirCli FchEnt FchPed fchven FlgEst FlgIgv FmaPgo Glosa LugEnt NomCli NroPed ordcmp RucCli Sede TpoCmb usuario RowNum RowIdent RowMod RowIdentIdx RowUserProp ChangedFields cContainerType hRowObject hDataQuery cDataFieldDefs FOR EACH  setRowObjectTable setRowObjUpdTable getUpdatableColumns REMOVE Update-Target PUSHROWOBJUPDTABLE pcValType PUSHTABLEANDVALIDATE pcContext pcMessages pcUndoIds obtainContextForClient REMOTECOMMIT SERVERCOMMIT GETROWOBJUPDSTATIC DISABLE_UI qDataQuery   x3  @  �A      / �<   ��      0         pcServices      ��      T         pcServices  �   ��      x         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow      ��              piStartRow  <  ��      ,        pcViewColList       ��      T       
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
 hTarget �  ��      �        pcMessage       ��      �        pcMessage       ��      �        plEnabled             �     cType       0     V   �                             getObjectType   S	  k	  m	  `        P  
   hReposBuffer    �        t  
   hPropTable  �        �  
   hBuffer           �  
   hTable  �  �     W   <          �                  adm-clone-props p  q  r  s  u  v  w  x  y  z  {  |    �  �  �  �            P  
   hProc             p        pcProcName  �  �  	   X   <  X      �                  start-super-proc    �  �  �  �  �  �  �  �  �           �     cTable               iTable  <        0     cColumns    \        P     cDataCols   |        p     cUpdCols    �        �     cCalcData   �        �     cCalcUpd    �     	   �     iNumData    �     
   �     iNumUpd              cBuffers    8        ,     cKeyFields  X        L     cAssignList x        l     iAssigns    �        �     iPos    �        �     iEntry  �        �     iCount  �        �     cTables        �     cTableAssign    ,              cDbEntry    H        @     cField  h        \     cKeyTable   �        |     cQueryString    �        �  
   hQuery  �        �  
   hBuffer �        �     cOpenQuery          �     cDBNames    (             cPhysicalTables T        <     cKeyTableEntityFields   �        h     cKeyTableEntityValues   �        �     cKeyTableEntityMnemonic �         �     cKeyTableEntityObjField �     !   �     cDBName      "        cEntityFields   <     #   ,     lHasObjectField \     $   P     lHasAudit   |     %   p     lHasComment �     &   �     lHasAutoComment �     '   �     iLookup        (   �     iAlias  |    3   Y   �                            initProps   p  q  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �    �  �  .  3  5  6  7  :  <  >  ?  B  C  I  W  �            �     lRet                      piTableIndex    �  h  *   Z   �  �      T                  deleteRecordStatic  �  �  �  �  �  �  �  �    	  %  &  B  C  _  `  |  }  �  �  �  �  �  �  �  �      *  +  G  H  d  e  �  �  �  �  �  �  �  �                 !       $  l     [             X                  pushRowObjUpdTable  %  �        �        pcValType                  $       (  �     \       p      �                  pushTableAndValidate    ;  <  >  $                pcContext   <             $       `        T        pcMessages            x        pcUndoIds   �  �     ]              �                  remoteCommit    g  i  m  q  s  �             $                       pcMessages            ,        pcUndoIds   �  x     ^       �      h                  serverCommit    �  �  8  �     _               �                  getRowObjUpdStatic  �  �  �       `               �                  disable_UI  )  *  �  �$  
     d      l$                      �  P  \     RowObject   �         �         �         �         �         �         �                                                        (         0         8         @         H         P         X         `         h         p         x         �         �         �         �         �         �         �         Atencion    Cmpbnte CodCia  CodCli  CodDiv  CodDoc  CodMon  CodPos  CodVen  DirCli  FchEnt  FchPed  fchven  FlgEst  FlgIgv  FmaPgo  Glosa   LugEnt  NomCli  NroPed  ordcmp  RucCli  Sede    TpoCmb  usuario RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp     �  �     RowObjUpd   T         `         h         p         x         �         �         �         �         �         �         �         �         �         �         �         �         �         �         �         �                                                        (         4         <         H         T         Atencion    Cmpbnte CodCia  CodCli  CodDiv  CodDoc  CodMon  CodPos  CodVen  DirCli  FchEnt  FchPed  fchven  FlgEst  FlgIgv  FmaPgo  Glosa   LugEnt  NomCli  NroPed  ordcmp  RucCli  Sede    TpoCmb  usuario RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp ChangedFields   �          x  
   appSrvUtils �       �     xiRocketIndexLimit  �        �  
   gshAstraAppserver   �        �  
   gshSessionManager                
   gshRIManager    H         4   
   gshSecurityManager  p         \   
   gshProfileManager   �   	 	     �   
   gshRepositoryManager    �   
 
     �   
   gshTranslationManager   �         �   
   gshWebManager   !         !     gscSessionId    4!        $!     gsdSessionObj   X!        H!  
   gshFinManager   |!        l!  
   gshGenManager   �!        �!  
   gshAgnManager   �!        �!     gsdTempUniqueID �!        �!     gsdUserObj  "        �!     gsdRenderTypeObj    4"         "     gsdSessionScopeObj  P"       H"  
   ghProp  p"       d"  
   ghADMProps  �"       �"  
   ghADMPropsBuf   �"       �"     glADMLoadFromRepos  �"       �"     glADMOk �"       �"  
   ghContainer #    	   #     cObjectName 4#    
   ,#     iStart  T#       H#     cAppService t#       h#     cASDivision �#       �#     cServerOperatingMode    �#       �#     cContainerType  �#       �#     cQueryString    $       �#  
   hRowObject  ($       $  
   hDataQuery  H$       <$     cColumns             \$     cDataFieldDefs  �$       |$  FacCPedi    �$    X  �$  RowObject         X  �$  RowObjUpd            9   �   �   �   �   !  "  #  $  ;  G  H  I  K  M  N  O  S  T  W  X  Y  Z  \  ^  `  b  c  d  g  i  j  l  m  n  o  p  v  x  ~  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  
  D
  E
  G
  H
  I
  J
  K
  L
  N
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
  h
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
  �
                     	  
              �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  0  �  �  �  �  �  �  �  �  �    !  3  R  T  i  �  	  
  $  4  5  6  9  :  ;  B  C  `  t  �  &  '  3  W  �  �  �  �  �  D  �  �  �  �  �  �  �  L  f  p  �  �  �  �  �  �  �  �      6  @  Z  |  �  �      ��  C:\Progress\OpenEdge\src\adm2\data.i �(  �) . %C:\Progress\OpenEdge\src\adm2\custom\datacustom.i    )  �� - C:\Progress\OpenEdge\src\adm2\robjflds.i X)  �� , d:\newsie\on_in_co\APLIC\vta2\dcotcredito.i  �)  �:  C:\Progress\OpenEdge\src\adm2\query.i    �)  z + C:\Progress\OpenEdge\src\adm2\delrecst.i �)  `W * C:\Progress\OpenEdge\src\adm2\tblprep.i  ,*  F� ) C:\Progress\OpenEdge\gui\fnarg   `*   ( %C:\Progress\OpenEdge\src\adm2\custom\querycustom.i   �*  �   C:\Progress\OpenEdge\src\adm2\dataquery.i    �*  �Z ' %C:\Progress\OpenEdge\src\adm2\custom\dataquerycustom.i   +  �< ! C:\Progress\OpenEdge\src\adm2\appserver.i    H+  �� & %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   �+  I� " C:\Progress\OpenEdge\src\adm2\smart.i    �+  Ds % C:\Progress\OpenEdge\gui\fn  �+  tw $ %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i    ,  Q. # C:\Progress\OpenEdge\gui\set `,  �>  C:\Progress\OpenEdge\src\adm2\dataprop.i �,  ��  %C:\Progress\OpenEdge\src\adm2\custom\datapropcustom.i    �,  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataprtocustom.i     -  �K  C:\Progress\OpenEdge\src\adm2\qryprop.i  D-  -�  %C:\Progress\OpenEdge\src\adm2\custom\qrypropcustom.i x-  ��  %C:\Progress\OpenEdge\src\adm2\custom\qryprtocustom.i �-   	 C:\Progress\OpenEdge\src\adm2\dataqueryprop.i    �-  �d  %C:\Progress\OpenEdge\src\adm2\custom\dataquerypropcustom.i   4.  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataqueryprtocustom.i   |.  �l  C:\Progress\OpenEdge\src\adm2\appsprop.i �.  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    �.  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    </  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i �/  �j  C:\Progress\OpenEdge\gui\get �/  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    �/  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i     0  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i d0  Su  C:\Progress\OpenEdge\src\adm2\globals.i  �0  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i �0  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   1  �  C:\Progress\OpenEdge\src\adm2\appsprto.i P1  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   �1  ��  C:\Progress\OpenEdge\src\adm2\dataqueryprto.i    �1  ª 
 %C:\Progress\OpenEdge\src\adm2\custom\dataquerydefscustom.i   2  ��  C:\Progress\OpenEdge\src\adm2\qryprto.i  P2  �  %C:\Progress\OpenEdge\src\adm2\custom\querydefscustom.i   �2  �`  C:\Progress\OpenEdge\src\adm2\dataprto.i �2  �  %C:\Progress\OpenEdge\src\adm2\custom\datadefscustom.i    �2  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   @3  ��   d:\newsie\on_in_co\APLIC\vta2\dcotcredito.w      �   �      �3  [  p     �3     n  %   �3  �   �     �3     �  .   �3  �   �      4     g     4  �   d      4     B  #   04  �   @     @4       #   P4  �        `4     �  #   p4  �   �     �4     �  #   �4  �   �     �4     �  #   �4  �   �     �4     �  #   �4  �   �     �4     h  #   �4  �   f      5     D  #   5  �   7      5       -   05  �        @5       ,   P5  k   �     `5  �  �     p5     �  +   �5  �  �     �5     �  +   �5  �  �     �5     g  +   �5  �  d     �5     J  +   �5  �  G     �5     -  +    6  �  *     6       +    6  �       06     �  +   @6  �  �     P6     �  +   `6  �  �     p6     �  +   �6  �  �     �6     �  +   �6  �  �     �6       +   �6  �  |     �6     b  +   �6  �  _     �6     E  +    7  �  B     7     (  +    7  �  %     07       +   @7  �       P7     �  +   `7  �  �     p7     �  +   �7  �  �     �7     �  +   �7  �  �     �7     r  #   �7  �  q     �7     O  #   �7  k  *     �7       #    8  j       8     �  #    8  i  �     08     �  #   @8  _  �     P8     �  *   `8  ^  �     p8     k  *   �8  ]  j     �8     D  *   �8  \  C     �8       *   �8  [       �8     �  *   �8  Z  �     �8     �  *    9  Y  �     9     �  *    9  X  �     09     �  *   @9  W  �     P9     Z  *   `9  V  Y     p9     3  *   �9  U  2     �9       *   �9  T       �9     �  *   �9  S  �     �9     �  *   �9  R  �     �9     �  *    :  Q  �     :     p  *    :  P  o     0:     I  *   @:  O  H     P:     "  *   `:  N  !     p:     �  *   �:  @  �     �:     �  #   �:  	  �     �:     �  )   �:  �   �     �:     _  #   �:  �   ^     �:     <  #    ;  �   ;     ;       #    ;  �        0;     �  #   @;  �   �     P;     �  #   `;  �   �     p;     �  #   �;  �   @     �;     �  (   �;  g   �     �;  a   �      �;     s  '   �;  _   q      �;     O  #   �;  ]   M       <     +  #   <  I          <  �     !   0<     �  &   @<  �   �  !   P<     �  #   `<  �   �  !   p<     l  #   �<  �   j  !   �<     H  #   �<  g   .  !   �<          �<  O   �  !   �<  �   �  "   �<       %   �<  �   O  "    =     �  $   =  �   �  "    =     �  #   0=  �   �  "   @=     �  #   P=  �   �  "   `=     �  #   p=  �   �  "   �=     a  #   �=  �   M  "   �=     +  #   �=  }     "   �=     �  #   �=     �  "   �=     3  !   �=     �       >     �     >     9      >  �   0     0>  O   "     @>          P>     �     `>  �   �     p>  �   �     �>  O   s     �>     b     �>          �>  y   �
     �>  �   �
  	   �>  G   �
     �>     �
     �>     |
      ?  c   
  	   ?  x   
      ?  M   �	     0?     �	     @?     �	     P?  a   �	     `?  �  j	     p?     K	     �?  �  	     �?  O   
	     �?     �     �?     �     �?  �   �     �?     �     �?     �     �?  x   �      @     �     @     f      @     b     0@     N     @@     5     P@  Q   %     `@     �     p@     �     �@          �@     e     �@  ]   _  	   �@     U     �@       	   �@     �  
   �@     �  	   �@  Z   �      A     �     A     �      A     �     0A     �     @A  c   i     PA     G     `A     �      pA     �      �A     �      �A     �      �A     !       �A           