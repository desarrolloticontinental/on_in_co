	��VE@[f?    �              �                                 �� 3F0C0143utf-8 MAIN D:\newsie\on_in_co\aplic\CCB\vnotacrdb-totales.w,, PROCEDURE disable_UI,, PROCEDURE displayObjects,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE startServerObject,, PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE restartServerObject,, PROCEDURE initializeServerObject,, PROCEDURE disconnectObject,, PROCEDURE destroyServerObject,, PROCEDURE bindServer,, PROCEDURE processAction,,INPUT pcAction CHARACTER PROCEDURE enableObject,, PROCEDURE disableObject,, PROCEDURE applyLayout,, PROCEDURE viewPage,,INPUT piPageNum INTEGER PROCEDURE viewObject,, PROCEDURE selectPage,,INPUT piPageNum INTEGER PROCEDURE removePageNTarget,,INPUT phTarget HANDLE,INPUT piPage INTEGER PROCEDURE passThrough,,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER PROCEDURE notifyPage,,INPUT pcProc CHARACTER PROCEDURE initPages,,INPUT pcPageList CHARACTER PROCEDURE initializeVisualContainer,, PROCEDURE hidePage,,INPUT piPageNum INTEGER PROCEDURE destroyObject,, PROCEDURE deletePage,,INPUT piPageNum INTEGER PROCEDURE createObjects,, PROCEDURE constructObject,,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE PROCEDURE changePage,, PROCEDURE assignPageProperty,,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER PROCEDURE validateFields,,INPUT-OUTPUT pcNotValidFields CHARACTER PROCEDURE updateTitle,, PROCEDURE updateRecord,, PROCEDURE updateMode,,INPUT pcMode CHARACTER PROCEDURE showDataMessagesProcedure,,OUTPUT pcReturn CHARACTER PROCEDURE resetRecord,, PROCEDURE queryPosition,,INPUT pcState CHARACTER PROCEDURE okToContinueProcedure,,INPUT pcAction CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE deleteRecord,, PROCEDURE dataAvailable,,INPUT pcRelative CHARACTER PROCEDURE confirmExit,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE confirmDelete,,INPUT-OUTPUT plAnswer LOGICAL PROCEDURE confirmContinue,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE collectChanges,,INPUT-OUTPUT pcChanges CHARACTER,INPUT-OUTPUT pcInfo CHARACTER PROCEDURE viewRecord,, PROCEDURE valueChanged,, PROCEDURE updateState,,INPUT pcState CHARACTER PROCEDURE toolbar,,INPUT pcValue CHARACTER PROCEDURE initializeObject,, PROCEDURE enableFields,, PROCEDURE displayFields,,INPUT pcColValues CHARACTER PROCEDURE disableFields,,INPUT pcFieldType CHARACTER PROCEDURE copyRecord,, PROCEDURE cancelRecord,, PROCEDURE addRecord,, FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION createUiEvents,LOGICAL, FUNCTION getObjectSecured,LOGICAL, FUNCTION getObjectTranslated,LOGICAL, FUNCTION setResizeVertical,LOGICAL,INPUT plResizeVertical LOGICAL FUNCTION setResizeHorizontal,LOGICAL,INPUT plResizeHorizontal LOGICAL FUNCTION setObjectLayout,LOGICAL,INPUT pcLayout CHARACTER FUNCTION setLayoutOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setHideOnInit,LOGICAL,INPUT plHide LOGICAL FUNCTION setDisableOnInit,LOGICAL,INPUT plDisable LOGICAL FUNCTION setDefaultLayout,LOGICAL,INPUT pcDefault CHARACTER FUNCTION setAllFieldNames,LOGICAL,INPUT pcValue CHARACTER FUNCTION setAllFieldHandles,LOGICAL,INPUT pcValue CHARACTER FUNCTION getResizeVertical,LOGICAL, FUNCTION getResizeHorizontal,LOGICAL, FUNCTION getWidth,DECIMAL, FUNCTION getRow,DECIMAL, FUNCTION getObjectLayout,CHARACTER, FUNCTION getObjectEnabled,LOGICAL, FUNCTION getLayoutVariable,CHARACTER, FUNCTION getLayoutOptions,CHARACTER, FUNCTION getHideOnInit,LOGICAL, FUNCTION getHeight,DECIMAL, FUNCTION getEnabledObjHdls,CHARACTER, FUNCTION getEnabledObjFlds,CHARACTER, FUNCTION getDisableOnInit,LOGICAL, FUNCTION getDefaultLayout,CHARACTER, FUNCTION getCol,DECIMAL, FUNCTION getAllFieldNames,CHARACTER, FUNCTION getAllFieldHandles,CHARACTER, FUNCTION setStatusArea,LOGICAL,INPUT plStatusArea LOGICAL FUNCTION setWindowTitleViewer,LOGICAL,INPUT phViewer HANDLE FUNCTION setWaitForObject,LOGICAL,INPUT phObject HANDLE FUNCTION setUpdateSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setTopOnly,LOGICAL,INPUT plTopOnly LOGICAL FUNCTION setSdoForeignFields,LOGICAL,INPUT cSdoForeignFields CHARACTER FUNCTION setSavedContainerMode,LOGICAL,INPUT cSavedContainerMode CHARACTER FUNCTION setRunMultiple,LOGICAL,INPUT plMultiple LOGICAL FUNCTION setRunDOOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setRouterTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setReEnableDataLinks,LOGICAL,INPUT cReEnableDataLinks CHARACTER FUNCTION setPrimarySdoTarget,LOGICAL,INPUT hPrimarySdoTarget HANDLE FUNCTION setPageSource,LOGICAL,INPUT phObject HANDLE FUNCTION setPageNTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setOutMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setNavigationTarget,LOGICAL,INPUT cTarget CHARACTER FUNCTION setNavigationSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setNavigationSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setMultiInstanceSupported,LOGICAL,INPUT lMultiInstanceSupported LOGICAL FUNCTION setMultiInstanceActivated,LOGICAL,INPUT lMultiInstanceActivated LOGICAL FUNCTION setInMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setFilterSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDynamicSDOProcedure,LOGICAL,INPUT pcProc CHARACTER FUNCTION setDisabledAddModeTabs,LOGICAL,INPUT cDisabledAddModeTabs CHARACTER FUNCTION setCurrentPage,LOGICAL,INPUT iPage INTEGER FUNCTION setContainerTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setCallerWindow,LOGICAL,INPUT h HANDLE FUNCTION setCallerProcedure,LOGICAL,INPUT h HANDLE FUNCTION setCallerObject,LOGICAL,INPUT h HANDLE FUNCTION pageNTargets,CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER FUNCTION getStatusArea,LOGICAL, FUNCTION getWindowTitleViewer,HANDLE, FUNCTION getWaitForObject,HANDLE, FUNCTION getUpdateSource,CHARACTER, FUNCTION getTopOnly,LOGICAL, FUNCTION getSdoForeignFields,CHARACTER, FUNCTION getSavedContainerMode,CHARACTER, FUNCTION getRunMultiple,LOGICAL, FUNCTION getRunDOOptions,CHARACTER, FUNCTION getReEnableDataLinks,CHARACTER, FUNCTION getPrimarySdoTarget,HANDLE, FUNCTION getPageSource,HANDLE, FUNCTION getPageNTarget,CHARACTER, FUNCTION getOutMessageTarget,HANDLE, FUNCTION getNavigationTarget,HANDLE, FUNCTION getNavigationSourceEvents,CHARACTER, FUNCTION getNavigationSource,CHARACTER, FUNCTION getMultiInstanceSupported,LOGICAL, FUNCTION getMultiInstanceActivated,LOGICAL, FUNCTION getFilterSource,HANDLE, FUNCTION getDynamicSDOProcedure,CHARACTER, FUNCTION getDisabledAddModeTabs,CHARACTER, FUNCTION getCurrentPage,INTEGER, FUNCTION getContainerTargetEvents,CHARACTER, FUNCTION getContainerTarget,CHARACTER, FUNCTION getContainerMode,CHARACTER, FUNCTION getCallerWindow,HANDLE, FUNCTION getCallerProcedure,HANDLE, FUNCTION enablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION disablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION showDataMessages,CHARACTER, FUNCTION setWindowTitleField,LOGICAL,INPUT cWindowTitleField CHARACTER FUNCTION setUpdateTargetNames,LOGICAL,INPUT pcTargetNames CHARACTER FUNCTION setUpdateTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setTableIOSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setTableIOSource,LOGICAL,INPUT phObject HANDLE FUNCTION setSaveSource,LOGICAL,INPUT plSave LOGICAL FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setLogicalObjectName,LOGICAL,INPUT pcLogicalObjectName CHARACTER FUNCTION setGroupAssignTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setGroupAssignTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setGroupAssignSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setGroupAssignSource,LOGICAL,INPUT phObject HANDLE FUNCTION setEnabledFields,LOGICAL,INPUT pcFieldList CHARACTER FUNCTION setDisplayedFields,LOGICAL,INPUT pcFieldList CHARACTER FUNCTION setDataModified,LOGICAL,INPUT plModified LOGICAL FUNCTION setContainerMode,LOGICAL,INPUT pcContainerMode CHARACTER FUNCTION okToContinue,LOGICAL,INPUT pcAction CHARACTER FUNCTION getWindowTitleField,CHARACTER, FUNCTION getUpdateTargetNames,CHARACTER, FUNCTION getUpdateTarget,CHARACTER, FUNCTION getTableIOSourceEvents,CHARACTER, FUNCTION getTableIOSource,HANDLE, FUNCTION getRowIdent,CHARACTER, FUNCTION getRecordState,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getNewRecord,CHARACTER, FUNCTION getGroupAssignTargetEvents,CHARACTER, FUNCTION getGroupAssignTarget,CHARACTER, FUNCTION getGroupAssignSourceEvents,CHARACTER, FUNCTION getGroupAssignSource,HANDLE, FUNCTION getFieldsEnabled,LOGICAL, FUNCTION getFieldHandles,CHARACTER, FUNCTION getEnabledHandles,CHARACTER, FUNCTION getEnabledFields,CHARACTER, FUNCTION getDisplayedTables,CHARACTER, FUNCTION getDisplayedFields,CHARACTER, FUNCTION getDataModified,LOGICAL, FUNCTION getCreateHandles,CHARACTER, FUNCTION setShowPopup,LOGICAL,INPUT plShowPopup LOGICAL FUNCTION getShowPopup,LOGICAL, FUNCTION getObjectType,character, FUNCTION getTargetProcedure,HANDLE, FUNCTION widgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION widgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION widgetIsTrue,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsModified,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetIsFocused,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsBlank,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetLongcharValue,LONGCHAR,INPUT pcName CHARACTER FUNCTION widgetHandle,HANDLE,INPUT pcName CHARACTER FUNCTION viewWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION toggleWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION resetWidgetValue,LOGICAL,INPUT pcNameList CHARACTER FUNCTION highlightWidget,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER FUNCTION hideWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION formattedWidgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION formattedWidgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION enableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION enableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION disableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION disableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION clearWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION blankWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION assignWidgetValueList,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION assignWidgetValue,LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER FUNCTION assignFocusedWidget,LOGICAL,INPUT pcName CHARACTER        h+              d#             R� h+  ��              |r              �/    +   �� �  U   �� `  V   �� �   Z   Ԟ t  ]           H� �  ? � J'  iSO8859-1                                                                           �*    �                                      �                    �                    p     �   V�    �             ��  �   0+      <+                                                         PROGRESS                                    
    
                    �              �                                                                                                     
  4         �          X  X)  ]   �)     �v      �)  ;                     x          (      �   �  q      ,  
    
                    �             �                                                                                          q          
  \  �      �  
    
                  �  �             H                                                                                          �          
    �      �  
    
                  p  8             �                                                                                          �          
  �  �      0  
    
                    �             �                                                                                          �          
  `  �      �  
    
                  �  �             L                                                                                          �          
    �      �  
    
                  t  <             �                                                                                          �          
  �  �      4  
    
                     �  	           �                                                                                          �          
  d  �      �  
    
                  �  �  
           P                                                                                          �          
           �                         x  @             �                                                                                                       �        8                        $  �             �                                                                                                      h	        �  
    
                  �  �	             T	                                                                                                    
  
  )      �	  
    
                  |	  D
              
                                                                                          )          
  �
  7      <
  
    
                  (
  �
             �
                                                                                          7          
  l  E      �
                        �
  �             X                                                                                          E              U      �                        �  H                                                                                                       U            �  `      @                        ,  �             �                                                                                          `                q      �                        �                 \                                                                                          q                          \�                                               `�          �  0  H X�            
             
             
                                         
                                                                                                                                                                        H   X   h   x   �   �   �   �   �   �   �   �       (  8  H      H   X   h   x   �   �   �   �   �   �   �   �      (  8  H                                                                                                                                     	                  
                                                                                                                                                                                                                                                                                                                                                                                                                               !                  "                  #                  $                  %                  &                  '                  (                  )                  *                  +                  ,                  -                  .                  /                  0                  1                  2                  3                  4                  5                  6                  7                  8                  9                  :                  ;                  <                                 x  �  �  �  �                         �  �  �  �  �                         �  �  �  �  �                         �  �  �  �  �                                                            $  ,  4  L  @                         P  X  d  |  l                         �  �  �  �  �                         �  �  �  �  �                         �  �     ,                             0   8   @   `   P                          d   l   t   �   |                          �   �   �   �   �                          �   �   �   �   �                          �   �   �    !  �                          !  !  !  $!  !                         (!  0!  8!  @!  <!                         D!  L!  T!  d!  \!                         h!  p!  x!  �!  �!                         �!  �!  �!  �!  �!          �!             �!  �!  �!  �!  �!                         �!  �!  �!   "  �!                         "  "  "   "  "                         $"  ,"  4"  L"  @"                         P"  X"  `"  x"  l"                         |"  �"  �"  �"  �"                         �"  �"  �"  �"  �"                         �"  �"  �"  #  �"                         #  #  #  @#  ,#                         D#  L#  \#  |#  l#                         �#  �#  �#  �#  �#                         �#  �#  �#  �#  �#          �#             �#  �#  $  $  $                          $  ($  8$  X$  H$                         \$  `$  h$  �$  x$          �$             �$  �$  �$  �$  �$                         �$  �$  �$  %  �$                         %  %  $%  D%  4%                         H%  P%  `%  �%  t%                         �%  �%  �%  �%                             �%  �%  �%  �%  �%                         �%  �%  &  $&  &                         (&  0&  @&  H&                             L&  X&  l&  x&                             |&  �&  �&  �&                             �&  �&  �&  �&  �&                         �&  �&  �&  '   '                         '  '  $'  4'  ,'                         8'  @'  P'  `'  X'                         d'  l'  |'  �'  �'                         �'  �'  �'  �'  �'                         �'  �'  �'  �'  �'                         �'  �'   (  (  (                         (  (  ,(  <(  4(                         @(  L(  \(  l(  d(                         p(  |(  �(  �(                             �(  �(  �(  �(                             �(  �(  �(   )                             )  $)  4)  T)                                                                         CodAnt  X(15)   DNI DNI     CodCli  x(11)   Cliente Cliente     CodCta  X(10)   Concepto    Cuenta      CodMon  9   Moneda  Moneda  1   CodRef  x(3)    Codigo  Codigo      DirCli  x(100)  Direccion   Direccion       FchAnu  99/99/99    Fecha   Fecha!Anulacion ?   FchDoc  99/99/9999  Fecha   Fecha   TODAY   FchVto  99/99/9999  Fecha de vencimiento    Fecha de!Vencimiento    ?   FmaPgo  X(8)    Condicion de ventas Condicion de!venta      Glosa   x(80)   Observaciones   Observaciones       NomCli  x(100)  Nombre  Nombre      NroDoc  X(15)   Numero  Numero      NroOrd  x(15)   Orden de Compra Orden de!Compra     NroPed  X(15)   Pedido  Pedido      NroRef  X(12)   Numero  Numero      RucCli  x(11)   Ruc Ruc     UsuAnu  X(10)   Usuario Usuario     usuario x(10)   usuario usuario     CodCia  999 Cia Cia 0   C�digo de compa�ia  CodDiv  x(5)    C.Div   C.Div   00000   CodDoc  x(3)    Codigo  Codigo      FlgEst  X   Estado  Estado  P   PorIgv  ->>9.99 % I.G.V.    % I.G.V.    0   PorDto  >>9.99  % Dscto.    % Dscto.    0   TpoCmb  Z,ZZ9.9999  Tipo de cambio  T/Cambio    0   Cndcre  X   Condicion de Credito    Condicion       TpoFac  X(1)    Tipo    Tipo        Tipo    x(20)   Tipo de documento   Tipo de documento       SdoAct  ->>,>>>,>>9.99  Importe Total   Importe Total   0   CodAlm  x(3)    Almacen Almacen     CodMov  99  C�digo de movimiento    C�digo!movimto. 0   C�digo de movimiento    CodVen  x(10)   Vendedor    Vendedor        ImpTot  ->>,>>>,>>9.99  Importe Total   Importe Total   0   CCo X(5)    Centro de Costo Centro!de Costo     Centro de Costo ImpIgv  ->>,>>>,>>9.99  Importe I.G.V.  Importe I.G.V.  0   ImpIsc  ->>,>>>,>>9.99  Importe Isc Importe Isc 0   ImpBrt  ->>,>>>,>>9.99  Importe Bruto   Importe Bruto   0   ImpDto  ->>,>>>,>>9.99  Importe Descuento   Importe Descuento   0   ImpDto2 ->>>,>>>,>>9.99 ImpDto2 0   ImpExo  ->>,>>>,>>9.99  Importe Exonerado   Importe Exonerado   0   ImpVta  ->>,>>>,>>9.99  Valor Venta Valor venta 0   imptot2 ->>>,>>>,>>9.99 imptot2 0   Libre_d02   ->>>,>>>,>>9.99<<<  Libre_d02   0   Dcto_Otros_VV   ->>>,>>>,>>9.999999 Dcto_Otros_VV   0   AcuBon1 ->>>,>>>,>>9.99 AcuBon  AcuBon  0   AcuBon2 ->>>,>>>,>>9.99 AcuBon  AcuBon  0   AcuBon3 ->>>,>>>,>>9.99 AcuBon  AcuBon  0   AcuBon4 ->>>,>>>,>>9.99 AcuBon  AcuBon  0   AcuBon5 ->>>,>>>,>>9.99 AcuBon  AcuBon  0   AcuBon6 ->>>,>>>,>>9.99 AcuBon  AcuBon  0   AcuBon7 ->>>,>>>,>>9.99 AcuBon  AcuBon  0   AcuBon8 ->>>,>>>,>>9.99 AcuBon  AcuBon  0   AcuBon9 ->>>,>>>,>>9.99 AcuBon  AcuBon  0   AcuBon10    ->>>,>>>,>>9.99 AcuBon  AcuBon  0   TotalVenta  >>>>>>>>>>>9.99 TotalVenta  0   TotalIGV    >>>>>>>>>>>9.99 TotalIGV    0   TotalMontoICBPER    >>>>>>>>>>>9.99 TotalMontoICBPER    0   TotalValorVentaNetoOpGravadas   >>>>>>>>>>>9.99 TotalValorVentaNetoOpGravadas   0   �    6 F�  ���=������     ���           00000 P                                    �       '                �     i    = 	       !   (   /   6   =   D   K   R   Y   `   f   m   t   {   �   �   �   �   �   �   �   �   �   �   �   �   �   �   �   �   �   �   �     
        &  .  5  <  D  N  \  d  l  t  |  �  �  �  �  �  �  �  �  �    ��                                               v          ����                            undefined                                                               �           �   l                             �����               ,;�                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D       p
     |          assignFocusedWidget         �      �     �      LOGICAL,INPUT pcName CHARACTER  assignWidgetValue   �      �      $          LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER  assignWidgetValueList         \      �          LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER  blankWidget t      �          ,      LOGICAL,INPUT pcNameList CHARACTER  clearWidget �      @      l    8      LOGICAL,INPUT pcNameList CHARACTER  disableRadioButton  L      �      �    D      LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    disableWidget   �            4    W      LOGICAL,INPUT pcNameList CHARACTER  enableRadioButton         X      �    e      LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    enableWidget    l      �      �    w      LOGICAL,INPUT pcNameList CHARACTER  formattedWidgetValue    �             X  	  �      CHARACTER,INPUT pcName CHARACTER    formattedWidgetValueList    8      |      �  
  �      CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    hideWidget  �      �      (   
 �      LOGICAL,INPUT pcNameList CHARACTER  highlightWidget       L      |    �      LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER  resetWidgetValue    \      �      �    �      LOGICAL,INPUT pcNameList CHARACTER  toggleWidget    �            H    �      LOGICAL,INPUT pcNameList CHARACTER  viewWidget  (      l      �   
 �      LOGICAL,INPUT pcNameList CHARACTER  widgetHandle    x      �      �    �      HANDLE,INPUT pcName CHARACTER   widgetLongcharValue �            @          LONGCHAR,INPUT pcName CHARACTER widgetIsBlank          `      �          LOGICAL,INPUT pcNameList CHARACTER  widgetIsFocused p      �      �    %      LOGICAL,INPUT pcName CHARACTER  widgetIsModified    �      	      8	    5      LOGICAL,INPUT pcNameList CHARACTER  widgetIsTrue    	      \	      �	    F      LOGICAL,INPUT pcName CHARACTER  widgetValue l	      �	      �	    S      CHARACTER,INPUT pcName CHARACTER    widgetValueList �	      �	      ,
    _      CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    d�    �  �
        T      4   ����T                                      ��                  �  �                  �N�                       �  �
  \  	  �  L                                        3   ����l      O   �  ��  ��  x  addRecord                                 �      ��                  A  B                \v�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            cancelRecord                                  �      ��                  D  E                 �v�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            copyRecord                                �      ��                  G  H                 ,2�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableFields                                 �      ��                  J  L  $              �2�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  <           ��                            ����                            displayFields                               8         ��                  N  P  P              �c�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  h           ��                            ����                            enableFields                                d  L      ��                  R  S  |              HJ�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                l  T      ��                  U  V  �              �J�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            toolbar                             h  P      ��                  X  Z  �              TK�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            updateState                             �  x      ��                  \  ^  �              $��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            valueChanged                                �  �      ��                  `  a  �               6�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewRecord                              �  �      ��                  c  d  �              �6�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            getTargetProcedure  
      ,      `    �      HANDLE, getObjectType   @      h      �          CHARACTER,  getShowPopup    x      �      �          LOGICAL,    setShowPopup    �      �          "      LOGICAL,INPUT plShowPopup LOGICAL   addRecord                               �  �      ��                  �  �  �              | �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            cancelRecord                                �  �      ��                  �  �  �              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            collectChanges                              �  �      ��                  �  �  �              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��   0             �               ��                  $           ��                            ����                            confirmContinue                                      ��                  �  �  8              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  P           ��                            ����                            confirmDelete                               L  4      ��                       d              �                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  |           ��                            ����                            confirmExit                             t  \      ��                      �              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            copyRecord                              �  �      ��                    	  �              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            dataAvailable                               �   �       ��                      �               ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �            ��                            ����                            deleteRecord                                �!  �!      ��                      �!              l��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �"  �"      ��                      �"               ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            okToContinueProcedure                               �#  �#      ��                      �#              |�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   D$             $               ��                  8$           ��                            ����                            queryPosition                               4%  %      ��                      L%              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  d%           ��                            ����                            resetRecord                             \&  D&      ��                      t&              `��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            showDataMessagesProcedure                               l'  T'      ��                  !  #  �'              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �'           ��                            ����                            updateMode                              �(  |(      ��                  %  '  �(              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �(           ��                            ����                            updateRecord                                �)  �)      ��                  )  *  �)              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateState                             �*  �*      ��                  ,  .  �*              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �*           ��                            ����                            updateTitle                             �+  �+      ��                  0  1   ,              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            validateFields                              �,  �,      ��                  3  5  -              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  -           ��                            ����                            getCreateHandles    �      �-      �-    /      CHARACTER,  getDataModified �-      �-      �-    @      LOGICAL,    getDisplayedFields  �-       .      4.    P      CHARACTER,  getDisplayedTables  .      @.      t.    c      CHARACTER,  getEnabledFields    T.      �.      �.     v      CHARACTER,  getEnabledHandles   �.      �.      �.  !  �      CHARACTER,  getFieldHandles �.       /      0/  "  �      CHARACTER,  getFieldsEnabled    /      </      p/  #  �      LOGICAL,    getGroupAssignSource    P/      |/      �/  $  �      HANDLE, getGroupAssignSourceEvents  �/      �/      �/  %  �      CHARACTER,  getGroupAssignTarget    �/      0      <0  &  �      CHARACTER,  getGroupAssignTargetEvents  0      H0      �0  '  �      CHARACTER,  getNewRecord    d0      �0      �0  (        CHARACTER,  getObjectParent �0      �0      �0  )  '      HANDLE, getRecordState  �0      1      41  *  7      CHARACTER,  getRowIdent 1      @1      l1  +  F      CHARACTER,  getTableIOSource    L1      x1      �1  ,  R      HANDLE, getTableIOSourceEvents  �1      �1      �1  -  c      CHARACTER,  getUpdateTarget �1      �1      (2  .  z      CHARACTER,  getUpdateTargetNames    2      42      l2  /  �      CHARACTER,  getWindowTitleField L2      x2      �2  0  �      CHARACTER,  okToContinue    �2      �2      �2  1  �      LOGICAL,INPUT pcAction CHARACTER    setContainerMode    �2      3      @3  2  �      LOGICAL,INPUT pcContainerMode CHARACTER setDataModified  3      h3      �3  3  �      LOGICAL,INPUT plModified LOGICAL    setDisplayedFields  x3      �3      �3  4  �      LOGICAL,INPUT pcFieldList CHARACTER setEnabledFields    �3      4      H4  5  �      LOGICAL,INPUT pcFieldList CHARACTER setGroupAssignSource    (4      l4      �4  6        LOGICAL,INPUT phObject HANDLE   setGroupAssignSourceEvents  �4      �4       5  7        LOGICAL,INPUT pcEvents CHARACTER    setGroupAssignTarget    �4      $5      \5  8  5      LOGICAL,INPUT pcObject CHARACTER    setGroupAssignTargetEvents  <5      �5      �5  9  J      LOGICAL,INPUT pcEvents CHARACTER    setLogicalObjectName    �5      �5      6  :  e      LOGICAL,INPUT pcLogicalObjectName CHARACTER setObjectParent �5      D6      t6  ;  z      LOGICAL,INPUT phParent HANDLE   setSaveSource   T6      �6      �6  <  �      LOGICAL,INPUT plSave LOGICAL    setTableIOSource    �6      �6      7  =  �      LOGICAL,INPUT phObject HANDLE   setTableIOSourceEvents  �6      87      p7  >  �      LOGICAL,INPUT pcEvents CHARACTER    setUpdateTarget P7      �7      �7  ?  �      LOGICAL,INPUT pcObject CHARACTER    setUpdateTargetNames    �7      �7       8  @  �      LOGICAL,INPUT pcTargetNames CHARACTER   setWindowTitleField  8      H8      |8  A  �      LOGICAL,INPUT cWindowTitleField CHARACTER   showDataMessages    \8      �8      �8  B  �      CHARACTER,  assignPageProperty                              �9  h9      ��                  7  :  �9              �iC                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �9             �9               ��                  �9           ��                            ����                            changePage                              �:  �:      ��                  <  =  �:              |mC                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmExit                             �;  �;      ��                  ?  A  �;              �|C                    O   ����    e�          O   ����    R�          O   ����    ��            ��                   <           ��                            ����                            constructObject                             �<  �<      ��                  C  H  =              ��C                    O   ����    e�          O   ����    R�          O   ����    ��            ��   `=             ,=               �� 
  �=             T=  
             ��   �=             |=               �� 
                 �=  
         ��                            ����                            createObjects                               �>  �>      ��                  J  K  �>              ��C                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deletePage                              �?  �?      ��                  M  O  �?              X�C                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �?           ��                            ����                            destroyObject                               �@  �@      ��                  Q  R  �@              �&C                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hidePage                                �A  �A      ��                  T  V  �A              �)C                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �A           ��                            ����                            initializeObject                                �B  �B      ��                  X  Y  C              8.C                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeVisualContainer                               D  �C      ��                  [  \  $D              X/C                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initPages                               E  �D      ��                  ^  `  $E              H2C                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  <E           ��                            ����                            notifyPage                              4F  F      ��                  b  d  LF              �2C                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  dF           ��                            ����                            passThrough                             \G  DG      ��                  f  i  tG              (GC                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �G             �G               ��                  �G           ��                            ����                            removePageNTarget                               �H  �H      ��                  k  n  �H              D�C                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  I             �H  
             ��                  I           ��                            ����                            selectPage                              J  �I      ��                  p  r  J              L�C                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  4J           ��                            ����                            toolbar                             (K  K      ��                  t  v  @K              ��C                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  XK           ��                            ����                            viewObject                              PL  8L      ��                  x  y  hL              ��C                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewPage                                PM  8M      ��                  {  }  hM              @�C                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �M           ��                            ����                            disablePagesInFolder    �8      �M       N  C  
      LOGICAL,INPUT pcPageInformation CHARACTER   enablePagesInFolder  N      LN      �N  D        LOGICAL,INPUT pcPageInformation CHARACTER   getCallerProcedure  `N      �N      �N  E  3      HANDLE, getCallerWindow �N      �N      O  F  F      HANDLE, getContainerMode    �N       O      TO  G  V      CHARACTER,  getContainerTarget  4O      `O      �O  H  g      CHARACTER,  getContainerTargetEvents    tO      �O      �O  I  z      CHARACTER,  getCurrentPage  �O      �O      P  J  �      INTEGER,    getDisabledAddModeTabs  �O      $P      \P  K  �      CHARACTER,  getDynamicSDOProcedure  <P      hP      �P  L  �      CHARACTER,  getFilterSource �P      �P      �P  M  �      HANDLE, getMultiInstanceActivated   �P      �P       Q  N  �      LOGICAL,    getMultiInstanceSupported    Q      ,Q      hQ  O  �      LOGICAL,    getNavigationSource HQ      tQ      �Q  P        CHARACTER,  getNavigationSourceEvents   �Q      �Q      �Q  Q  (      CHARACTER,  getNavigationTarget �Q      �Q      0R  R  B      HANDLE, getOutMessageTarget R      8R      lR  S  V      HANDLE, getPageNTarget  LR      tR      �R  T  j      CHARACTER,  getPageSource   �R      �R      �R  U  y      HANDLE, getPrimarySdoTarget �R      �R      S  V  �      HANDLE, getReEnableDataLinks    �R      $S      \S  W  �      CHARACTER,  getRunDOOptions <S      hS      �S  X  �      CHARACTER,  getRunMultiple  xS      �S      �S  Y  �      LOGICAL,    getSavedContainerMode   �S      �S      T  Z  �      CHARACTER,  getSdoForeignFields �S      $T      XT  [  �      CHARACTER,  getTopOnly  8T      dT      �T  \ 
 �      LOGICAL,    getUpdateSource pT      �T      �T  ]  	      CHARACTER,  getWaitForObject    �T      �T      U  ^  	      HANDLE, getWindowTitleViewer    �T      U      LU  _  %	      HANDLE, getStatusArea   ,U      TU      �U  `  :	      LOGICAL,    pageNTargets    dU      �U      �U  a  H	      CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER setCallerObject �U      �U      (V  b  U	      LOGICAL,INPUT h HANDLE  setCallerProcedure  V      @V      tV  c  e	      LOGICAL,INPUT h HANDLE  setCallerWindow TV      �V      �V  d  x	      LOGICAL,INPUT h HANDLE  setContainerTarget  �V      �V      W  e  �	      LOGICAL,INPUT pcObject CHARACTER    setCurrentPage  �V      ,W      \W  f  �	      LOGICAL,INPUT iPage INTEGER setDisabledAddModeTabs  <W      xW      �W  g  �	      LOGICAL,INPUT cDisabledAddModeTabs CHARACTER    setDynamicSDOProcedure  �W      �W      X  h  �	      LOGICAL,INPUT pcProc CHARACTER  setFilterSource �W      8X      hX  i  �	      LOGICAL,INPUT phObject HANDLE   setInMessageTarget  HX      �X      �X  j  �	      LOGICAL,INPUT phObject HANDLE   setMultiInstanceActivated   �X      �X      Y  k  �	      LOGICAL,INPUT lMultiInstanceActivated LOGICAL   setMultiInstanceSupported   �X      HY      �Y  l  
      LOGICAL,INPUT lMultiInstanceSupported LOGICAL   setNavigationSource dY      �Y      �Y  m  /
      LOGICAL,INPUT pcSource CHARACTER    setNavigationSourceEvents   �Y      Z      HZ  n  C
      LOGICAL,INPUT pcEvents CHARACTER    setNavigationTarget (Z      lZ      �Z  o  ]
      LOGICAL,INPUT cTarget CHARACTER setOutMessageTarget �Z      �Z      �Z  p  q
      LOGICAL,INPUT phObject HANDLE   setPageNTarget  �Z      [      D[  q  �
      LOGICAL,INPUT pcObject CHARACTER    setPageSource   $[      h[      �[  r  �
      LOGICAL,INPUT phObject HANDLE   setPrimarySdoTarget x[      �[      �[  s  �
      LOGICAL,INPUT hPrimarySdoTarget HANDLE  setReEnableDataLinks    �[      \      L\  t  �
      LOGICAL,INPUT cReEnableDataLinks CHARACTER  setRouterTarget ,\      x\      �\  u  �
      LOGICAL,INPUT phObject HANDLE   setRunDOOptions �\      �\      �\  v  �
      LOGICAL,INPUT pcOptions CHARACTER   setRunMultiple  �\      ]      L]  w  �
      LOGICAL,INPUT plMultiple LOGICAL    setSavedContainerMode   ,]      p]      �]  x  �
      LOGICAL,INPUT cSavedContainerMode CHARACTER setSdoForeignFields �]      �]      ^  y        LOGICAL,INPUT cSdoForeignFields CHARACTER   setTopOnly  �]      4^      `^  z 
 $      LOGICAL,INPUT plTopOnly LOGICAL setUpdateSource @^      �^      �^  {  /      LOGICAL,INPUT pcSource CHARACTER    setWaitForObject    �^      �^      _  |  ?      LOGICAL,INPUT phObject HANDLE   setWindowTitleViewer    �^      (_      `_  }  P      LOGICAL,INPUT phViewer HANDLE   setStatusArea   @_      �_      �_  ~  e      LOGICAL,INPUT plStatusArea LOGICAL  applyLayout                             d`  L`      ��                  �  �  |`              D�@                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableObject                               ha  Pa      ��                  �  �  �a              ��@                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            enableObject                                lb  Tb      ��                  �     �b              H�@                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                tc  \c      ��                      �c              X�@                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            processAction                               xd  `d      ��                      �d              P�@                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �d           ��                            ����                            getAllFieldHandles  �_      e      De    s      CHARACTER,  getAllFieldNames    $e      Pe      �e  �  �      CHARACTER,  getCol  de      �e      �e  �  �      DECIMAL,    getDefaultLayout    �e      �e      �e  �  �      CHARACTER,  getDisableOnInit    �e      f      8f  �  �      LOGICAL,    getEnabledObjFlds   f      Df      xf  �  �      CHARACTER,  getEnabledObjHdls   Xf      �f      �f  �  �      CHARACTER,  getHeight   �f      �f      �f  � 	 �      DECIMAL,    getHideOnInit   �f      �f      ,g  �  �      LOGICAL,    getLayoutOptions    g      8g      lg  �  �      CHARACTER,  getLayoutVariable   Lg      xg      �g  �        CHARACTER,  getObjectEnabled    �g      �g      �g  �        LOGICAL,    getObjectLayout �g      �g      (h  �  0      CHARACTER,  getRow  h      4h      \h  �  @      DECIMAL,    getWidth    <h      hh      �h  �  G      DECIMAL,    getResizeHorizontal th      �h      �h  �  P      LOGICAL,    getResizeVertical   �h      �h      i  �  d      LOGICAL,    setAllFieldHandles  �h       i      Ti  �  v      LOGICAL,INPUT pcValue CHARACTER setAllFieldNames    4i      ti      �i  �  �      LOGICAL,INPUT pcValue CHARACTER setDefaultLayout    �i      �i      �i  �  �      LOGICAL,INPUT pcDefault CHARACTER   setDisableOnInit    �i       j      Tj  �  �      LOGICAL,INPUT plDisable LOGICAL setHideOnInit   4j      tj      �j  �  �      LOGICAL,INPUT plHide LOGICAL    setLayoutOptions    �j      �j      �j  �  �      LOGICAL,INPUT pcOptions CHARACTER   setObjectLayout �j      k      Lk  �  �      LOGICAL,INPUT pcLayout CHARACTER    setResizeHorizontal ,k      pk      �k  �  �      LOGICAL,INPUT plResizeHorizontal LOGICAL    setResizeVertical   �k      �k      l  �  �      LOGICAL,INPUT plResizeVertical LOGICAL  getObjectTranslated �k      ,l      `l  �        LOGICAL,    getObjectSecured    @l      ll      �l  �  %      LOGICAL,    createUiEvents  �l      �l      �l  �  6      LOGICAL,    bindServer                              xm  `m      ��                  �  �  �m              �@                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               |n  dn      ��                  �  �  �n              PZA                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �o  lo      ��                  �  �  �o              \[A                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �p  tp      ��                  �  �  �p              `bA                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �q  �q      ��                  �  �  �q              �bA                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �r  �r      ��                  �  �  �r              �cA                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �s  �s      ��                  �  �  �s              T�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �s  
         ��                            ����                            startServerObject                               �t  �t      ��                  �     �t              <�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                �u  �u      ��                      �u              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  v           ��                            ����                            getAppService   �l      pv      �v  �  E      CHARACTER,  getASBound  �v      �v      �v  � 
 S      LOGICAL,    getAsDivision   �v      �v      w  �  ^      CHARACTER,  getASHandle �v       w      Lw  �  l      HANDLE, getASHasStarted ,w      Tw      �w  �  x      LOGICAL,    getASInfo   dw      �w      �w  � 	 �      CHARACTER,  getASInitializeOnRun    �w      �w       x  �  �      LOGICAL,    getASUsePrompt  �w      x      <x  �  �      LOGICAL,    getServerFileName   x      Hx      |x  �  �      CHARACTER,  getServerOperatingMode  \x      �x      �x  �  �      CHARACTER,  runServerProcedure  �x      �x       y  �  �      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   �x      Dy      ty  �  �      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   Ty      �y      �y  �         LOGICAL,INPUT pcDivision CHARACTER  setASHandle �y      �y      z  �        LOGICAL,INPUT phASHandle HANDLE setASInfo   �y      <z      hz  � 	       LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    Hz      �z      �z  �  $      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �z      �z      {  �  9      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   �z      4{      h{  �  H      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  H{      �{      �{  �  Z      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �|  h|      ��                  �  �  �|              ��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �|             �|  
             ��   }             �|               �� 
                  }  
         ��                            ����                            addMessage                              �}  �}      ��                  �  �  ~              d�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   \~             (~               ��   �~             P~               ��                  x~           ��                            ����                            adjustTabOrder                              t  \      ��                  �  �  �              ��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �             �  
             �� 
   �             �  
             ��                  �           ��                            ����                            applyEntry                              �  Ԁ      ��                  �  �  �              �                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            changeCursor                                �   �      ��                  �  �  0�              (�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  H�           ��                            ����                            createControls                              D�  ,�      ��                  �  �  \�              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               H�  0�      ��                  �  �  `�              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                L�  4�      ��                  �  �  d�              @                     O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              X�  @�      ��                  �  �  p�              �                     O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              X�  @�      ��                  �  �  p�              0                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              X�  @�      ��                  �  �  p�              �0                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                `�  H�      ��                  �  �  x�              H1                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              h�  P�      ��                  �  �  ��              ��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  ̊             ��  
             ��   �             ��               ��   �             �               ��                  �           ��                            ����                            modifyUserLinks                             �  �      ��                  �  	  $�              �                    O   ����    e�          O   ����    R�          O   ����    ��            ��   p�             <�               ��   ��             d�               �� 
                 ��  
         ��                            ����                            removeAllLinks                              ��  p�      ��                  	  	  ��               �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              ��  p�      ��                  	  
	  ��              �h                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �             ��  
             ��   �             ��               �� 
                 �  
         ��                            ����                            repositionObject                                �  ��      ��                  	  	   �              �n                    O   ����    e�          O   ����    R�          O   ����    ��            ��   l�             8�               ��                  `�           ��                            ����                            returnFocus                             X�  @�      ��                  	  	  p�              <t                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 ��  
         ��                            ����                            showMessageProcedure                                ��  t�      ��                  	  	  ��              Ȑ                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             ��               ��                  �           ��                            ����                            toggleData                              ܓ  ē      ��                  	  	  ��              PF                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            viewObject                              �  �      ��                  	  	  �              X�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �{      t�      ��  � 
 �      LOGICAL,    assignLinkProperty  ��      ��      ��  �  �      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   ��      8�      h�  �  �      CHARACTER,  getChildDataKey H�      t�      ��  �  �      CHARACTER,  getContainerHandle  ��      ��      �  �  �      HANDLE, getContainerHidden  Ė      �       �  �        LOGICAL,    getContainerSource   �      ,�      `�  �  !      HANDLE, getContainerSourceEvents    @�      h�      ��  �  4      CHARACTER,  getContainerType    ��      ��      �  �  M      CHARACTER,  getDataLinksEnabled ė      �      $�  �  ^      LOGICAL,    getDataSource   �      0�      `�  �  r      HANDLE, getDataSourceEvents @�      h�      ��  �  �      CHARACTER,  getDataSourceNames  |�      ��      ܘ  �  �      CHARACTER,  getDataTarget   ��      �      �  �  �      CHARACTER,  getDataTargetEvents ��      $�      X�  �  �      CHARACTER,  getDBAware  8�      d�      ��  � 
 �      LOGICAL,    getDesignDataObject p�      ��      Й  �  �      CHARACTER,  getDynamicObject    ��      ܙ      �  �  �      LOGICAL,    getInstanceProperties   �      �      T�  �  �      CHARACTER,  getLogicalObjectName    4�      `�      ��  �        CHARACTER,  getLogicalVersion   x�      ��      ؚ  �  $      CHARACTER,  getObjectHidden ��      �      �  �  6      LOGICAL,    getObjectInitialized    ��       �      X�  �  F      LOGICAL,    getObjectName   8�      d�      ��  �  [      CHARACTER,  getObjectPage   t�      ��      Л  �  i      INTEGER,    getObjectVersion    ��      ܛ      �  �  w      CHARACTER,  getObjectVersionNumber  �      �      T�  �  �      CHARACTER,  getParentDataKey    4�      `�      ��  �  �      CHARACTER,  getPassThroughLinks t�      ��      Ԝ  �  �      CHARACTER,  getPhysicalObjectName   ��      ��      �  �  �      CHARACTER,  getPhysicalVersion  ��      $�      X�  �  �      CHARACTER,  getPropertyDialog   8�      d�      ��  �  �      CHARACTER,  getQueryObject  x�      ��      ԝ  �  �      LOGICAL,    getRunAttribute ��      ��      �  �        CHARACTER,  getSupportedLinks   �      �      P�  �        CHARACTER,  getTranslatableProperties   0�      \�      ��  �  0      CHARACTER,  getUIBMode  x�      ��      О  � 
 J      CHARACTER,  getUserProperty ��      ܞ      �  �  U      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    �      4�      l�  �  e      CHARACTER,INPUT pcPropList CHARACTER    linkHandles L�      ��      ��  �  z      CHARACTER,INPUT pcLink CHARACTER    linkProperty    ��      �      �  �  �      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry ��      P�      |�  �  �      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   \�      �      �  �  �      CHARACTER,INPUT piMessage INTEGER   propertyType    ��      <�      l�  �  �      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  L�      ��      ġ  �  �      CHARACTER,  setChildDataKey ��      С       �  �  �      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  �      (�      \�  �  �      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  <�      |�      ��  �  �      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    ��      Т      �  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled �      0�      d�  �        LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   D�      ��      ��  �  ,      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents ��      ܣ      �  �  :      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  �      8�      l�  �  N      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   L�      ��      Ĥ  �  a      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents ��      �      �  �  o      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  ��      @�      l�  � 
 �      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject L�      ��      ��  �  �      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    ��      �      �  �  �      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   ��      8�      p�  �  �      LOGICAL,INPUT pcPropList CHARACTER  setLogicalVersion   P�      ��      Ȧ  �  �      LOGICAL,INPUT cVersion CHARACTER    setObjectName   ��      �      �  �  �      LOGICAL,INPUT pcName CHARACTER  setObjectVersion    ��      <�      p�  �  �      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    P�      ��      ̧  �  �      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks ��      ��      (�  �        LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   �      H�      ��  �        LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  `�      ��      Ԩ  �  5      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute ��      ��      (�  �  H      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   �      P�      ��  �  X      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   d�      ��      �  �  j      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  ĩ      �      4�  � 
 �      LOGICAL,INPUT pcMode CHARACTER  setUserProperty �      T�      ��  �  �      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage d�      Ī      �  �  �      LOGICAL,INPUT pcMessage CHARACTER   Signature   Ъ      �      @�  � 	 �      CHARACTER,INPUT pcName CHARACTER    8�    5
  ��  ��      �      4   �����                �                      ��                  6
  c
                  �Y7                       6
  ��        7
  (�  ��      �      4   �����                ��                      ��                  8
  b
                  Z7                       8
  8�  ��    O
  Ь  L�      �      4   �����                \�                      ��                  [
  ]
                  �Z7                       [
  �         \
                                  T     
                    � ߱        �  $  _
  ��  ���                           $  a
  �  ���                       �                         � ߱        D�    g
  T�  Ю      �      4   �����                �                      ��                  h
  ,                  $�7                       h
  d�  �  o   k
      ,                                 l�  $   l
  @�  ���                       $  @                       � ߱        ��  �   m
  D      ��  �   n
  �      ��  �   p
  ,      ��  �   r
  �      Я  �   t
        �  �   v
  �      ��  �   w
        �  �   x
  @       �  �   {
  �      4�  �   }
  (      H�  �   ~
  �      \�  �   �
         p�  �   �
  �      ��  �   �
  �      ��  �   �
  T	      ��  �   �
  �	      ��  �   �
  
      ԰  �   �
  x
      �  �   �
  �
      ��  �   �
  (      �  �   �
  �      $�  �   �
        8�  �   �
  �      L�  �   �
        `�  �   �
  �      t�  �   �
  �      ��  �   �
  l      ��  �   �
  �      ��  �   �
        ı  �   �
  X      ر  �   �
  �      �  �   �
         �  �   �
  D      �  �   �
  �      (�  �   �
  �      <�  �   �
  8      P�  �   �
  t      d�  �   �
  �      x�  �   �
  �      ��  �   �
  (      ��  �   �
  d      ��  �   �
  �      Ȳ  �   �
  �      ܲ  �   �
            �   �
  T                      �          t�  \�      ��                  S  �  ��              ��7                    O   ����    e�          O   ����    R�          O   ����    ��      �     
                @                     P                         � ߱        4�  $ g  ��  ���                           O     ��  ��  �               ��          ��  ��    ��                                             ��                            ����                                h      �      L�     T     ��                       ��                       �    �  `�  ܵ      �      4   �����                �                      ��                  �  (                  ��7                       �  p�   �  �   �  �      �  �   �  p      (�  �   �  �      <�  �   �  h      P�  �   �  �      d�  �   �  `      x�  �   �  �      ��  �   �  P      ��  �   �  �      ��  �   �  H      ȶ  �   �  �      ܶ  �   �  8      �  �   �  �          �   �  0      ܹ    3   �  ��      �      4   �����                ��                      ��                  4  �                  h�7                       4  0�  ��  �   6         Է  �   7  t      �  �   8  �      ��  �   9  d      �  �   :  �      $�  �   ;  L      8�  �   <  �      L�  �   =  <       `�  �   >  �       t�  �   ?  $!      ��  �   @  �!      ��  �   A  "      ��  �   B  �"      ĸ  �   C  #      ظ  �   D  �#      �  �   E  �#       �  �   F  x$      �  �   G  �$      (�  �   H  p%      <�  �   I  �%      P�  �   J  h&      d�  �   K  �&      x�  �   L  `'      ��  �   M  �'      ��  �   N  X(      ��  �   O  �(      ȹ  �   P  P)          �   Q  �)      ��    �  ��  t�      4*      4   ����4*                ��                      ��                  �  �                  ��7                       �  �  ��  �   �  �*      ��  �   �  +      ��  �   �  �+      Ժ  �   �   ,      �  �   �  t,      ��  �   �  �,      �  �   �  \-      $�  �   �  �-      8�  �   �  .      L�  �   �  H.      `�  �   �  �.      t�  �   �  �.      ��  �   �  l/      ��  �   �  �/      ��  �   �  \0      Ļ  �   �  �0      ػ  �   �  D1      �  �   �  �1       �  �   �  <2      �  �   �  x2      (�  �   �  �2      <�  �   �  `3      P�  �   �  �3      d�  �   �  4      x�  �   �  L4      ��  �   �  �4      ��  �   �  5      ��  �   �  @5      ȼ  �   �  |5      ܼ  �   �  �5      �  �   �  �5      �  �   �  06      �  �   �  l6      ,�  �   �  �6      @�  �   �  7      T�  �   �  X7      h�  �   �  �7      |�  �   �  �7      ��  �   �  8      ��  �   �  H8      ��  �   �  �8      ̽  �      �8      �  �     l9      ��  �     �9      �  �     T:      �  �     �:      0�  �     L;      D�  �     �;      X�  �     D<      l�  �     �<      ��  �   	  <=      ��  �   
  x=      ��  �     �=      ��  �     0>      о  �     l>      �  �     �>          �     ?      �    �  �  ��      �?      4   �����?  	              ��                      ��             	     �  )                  8                       �  $�  ��  �   �  �?      ȿ  �   �  X@      ܿ  �   �  �@      �  �   �  HA      �  �   �  �A      �  �   �  XB      ,�  �   �  �B      @�  �   �  @C      T�  �   �  �C      h�  �   �  8D      |�  �   �  �D      ��  �   �  (E      ��  �   �  dE      ��  �   �  �E      ��  �   �  LF      ��  �   �  �F      ��  �   �  4G      �  �   �  �G      �  �   �  H      0�  �   �  �H      D�  �   �  I      X�  �   �  �I      l�  �   �  �I      ��  �   �  pJ      ��  �   �  �J      ��  �   �   K      ��  �   �  �K      ��  �   �  L      ��  �   �  �L      ��  �   �  �L          �   �  tM      ��    4  (�  ��      �M      4   �����M  
              ��                      ��             
     5  �                  H!8                       5  8�  ��  �   7  N      ��  �   8  �N          �   9  �N      ��    n  �  ��      ,O      4   ����,O                ��                      ��                  o  x                  `8                       o  �  �    q  ��  ��      DO      4   ����DO      $  r  ��  ���                       �O  @         tO              � ߱              u  8�  H�      �O      4   �����O      $  v  t�  ���                       �O  @         �O              � ߱        ��  $  �  ��  ���                       ,P     
                    � ߱        ��    �  �  $�      @P      4   ����@P      /   �  P�     `�                          3   ����PP            ��                      3   ����pP  ��    �  ��  (�  �  �P      4   �����P                8�                      ��                  �  H                  ��7                       �  ��  L�  �   �  �P      ��  $  �  x�  ���                       Q     
                    � ߱        ��  �   �  8Q      �  $   �  ��  ���                       `Q  @         LQ              � ߱        ��  $  �  <�  ���                       �Q                         � ߱        �R     
                �R                     LT  @        
 T              � ߱        \�  V   �  h�  ���                        XT                     �T       	       	       �T                         � ߱        ��  $  �  ��  ���                       �U     
                V                     TW  @        
 W              � ߱        |�  V     ��  ���                        `W     
                �W                     ,Y  @        
 �X              � ߱            V   ,  �  ���                                      ��                      ��                  J  �                  ��7                       J  ��  @Y     
                �Y                     [  @        
 �Z          t[  @        
 4[          �[  @        
 �[          4\  @        
 �[              � ߱            V   _  $�  ���                        adm-clone-props �  �              �     U     `                          \  j$                     start-super-proc    �  t�  �           �     V                                  �$                     |�    �   �  �      �_      4   �����_      /      <�     L�                          3   �����_            l�                      3   �����_  ��  $    ��  ���                       `       
       
           � ߱        ��    *  ��  l�  �  ,`      4   ����,`                ��                      ��                  +  /                  ��                       +   �  @`       
       
       T`                     h`                         � ߱            $  ,  |�  ���                             0  (�  d�      �`      4   �����`  �`       
       
           � ߱            $  1  8�  ���                       ��    8  ��  ��  �  �`      4   �����`      $  9  ��  ���                       �`                         � ߱            �   V  �`      (a     
                �a                     �b  @        
 �b              � ߱        ��  V   j  (�  ���                        ��  �   �   c      d�      ��  ��      @c      4   ����@c      /      $�     4�                          3   ����Pc            T�                      3   ����pc   �  $  $  ��  ���                       �c                         � ߱        �c     
                4d                     �e  @        
 De              � ߱        L�  V   .  ��  ���                        ,�    �  h�  ��      �e      4   �����e                ��                      ��                  �  �                  �                       �  x�      g   �  �         ���                           ��          ��  ��      ��                  �      ��              x�                    O   ����    e�          O   ����    R�          O   ����    ��          /  �   �     �  �e                      3   �����e  @�     
   0�                      3   �����e         
   `�                      3   �����e    ��                              ��        v                  ����                                         �              W      p�                      g                               4�  g   �  D�          �	��                           �          ��  ��      ��                  �  �  ��              �                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  8�     H�  �e                      3   �����e            h�                      3   �����e    ��                              ��        v                  ����                                        X�              X      x�                      g                               <�  g   �  L�          �	��                           �          ��  ��      ��                  �  �  ��              x�                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  @�     P�  0f                      3   ����f            p�                      3   ����8f    ��                              ��        v                  ����                                        `�              Y      ��                      g                               ��    �  X�  ��      Tf      4   ����Tf                ��                      ��                  �  �                  �                       �  h�  P�  /   �  �      �                          3   ����df            @�                      3   �����f  L�  /  �  |�     ��  �f                      3   �����f  ��     
   ��                      3   �����f  ��        ��                      3   �����f  �        �                      3   �����f            <�                      3   ����g  t�    �  h�  x�      ,g      4   ����,g      /  �  ��     ��  �g                      3   �����g  ��     
   ��                      3   �����g  �        �                      3   �����g  D�        4�                      3   �����g            d�                      3   �����g        �  ��  ��      h      4   ����h      /  �  ��     ��  ph                      3   ����Ph  �     
   ��                      3   ����xh  <�        ,�                      3   �����h  l�        \�                      3   �����h            ��                      3   �����h  4�     �  �h                                     �h     
                di                     �j  @        
 tj              � ߱        ��  V   f  ��  ���                        �j     
                Dk                     �l  @        
 Tl              � ߱        ��  V   �  `�  ���                        l�    �  �  ��      �l      4   �����l                ��                      ��                  �  �                  ��                       �  �  �  /   �  ��     ��                          3   �����l            ��                      3   �����l      /   �  0�     @�                          3   �����l  p�     
   `�                      3   ����m  ��        ��                      3   ����m  ��        ��                      3   ����0m            ��                      3   ����Lm  displayObjects  ��   �                      Z      �                               �%                     D�  g   i  ��         4��                           L�          �  �      ��                  j      4�              ��                    O   ����    e�          O   ����    R�          O   ����    ��          /  j  x�         �m                      3   ����hm    ��                              ��        v                  ����                                        ��              [      ��                      g                               ��  g   o  \�          0��      }                      $�          ��  ��      ��                  p      �              �                    O   ����    e�          O   ����    R�          O   ����    ��          /  p  P�         �m                      3   �����m    ��                            ����                                        p�              \      `�                      g                               ��    s  �  ��      �m      4   �����m                ��                      ��                  t  {                  ��                       t  (�  �  /   u  ��     ��                          3   �����m             �                      3   �����m      /  v  <�     L�  n                      3   �����m  |�     
   l�                      3   ����$n  ��        ��                      3   ����,n  ��        ��                      3   ����@n            ��                      3   ����`n  �n                     �n                     �n                     ,o                         � ߱        T�  $  �  �  ���                       �o     
                �o                     Lq  @        
 q          �q  @        
 dq          �q  @        
 �q              � ߱        ��  V   �  ��  ���                        $r  @         r          Lr  @         8r              � ߱            $   �  ��  ���                       disable_UI  �  ��                      ]                                    	'  
                    �� �   ���  �                ��  ��      toggleData  ,INPUT plEnabled LOGICAL    x�  ��  ��      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  ��  �  (�      returnFocus ,INPUT hTarget HANDLE   �  P�  d�      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    @�  ��  ��      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��   �  �      removeAllLinks  ,   ��  $�  4�      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE �  ��  ��      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    |�  �  $�      hideObject  ,   �  8�  D�      exitObject  ,   (�  X�  p�      editInstanceProperties  ,   H�  ��  ��      displayLinks    ,   t�  ��  ��      createControls  ,   ��  ��  ��      changeCursor    ,INPUT pcCursor CHARACTER   ��  �  �      applyEntry  ,INPUT pcField CHARACTER    ��  @�  P�      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER 0�  ��  ��      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER ��  �  �      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  h�  x�      unbindServer    ,INPUT pcMode CHARACTER X�  ��  ��      startServerObject   ,   ��  ��  ��      runServerObject ,INPUT phAppService HANDLE  ��  �  �      restartServerObject ,   ��  ,�  D�      initializeServerObject  ,   �  X�  l�      disconnectObject    ,   H�  ��  ��      destroyServerObject ,   p�  ��  ��      bindServer  ,   ��  ��  ��      processAction   ,INPUT pcAction CHARACTER   ��  �  �      enableObject    ,   ��  (�  8�      disableObject   ,   �  L�  X�      applyLayout ,   <�  l�  x�      viewPage    ,INPUT piPageNum INTEGER    \�  ��  ��      viewObject  ,   ��  ��  ��      selectPage  ,INPUT piPageNum INTEGER    ��  ��  �      removePageNTarget   ,INPUT phTarget HANDLE,INPUT piPage INTEGER ��  L�  X�      passThrough ,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER  <�  ��  ��      notifyPage  ,INPUT pcProc CHARACTER ��  ��  ��      initPages   ,INPUT pcPageList CHARACTER ��  �  (�      initializeVisualContainer   ,   ��  <�  H�      hidePage    ,INPUT piPageNum INTEGER    ,�  t�  ��      destroyObject   ,   d�  ��  ��      deletePage  ,INPUT piPageNum INTEGER    ��  ��  ��      createObjects   ,   ��  ��  �      constructObject ,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE ��  x�  ��      changePage  ,   h�  ��  ��      assignPageProperty  ,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER ��  ��  ��      validateFields  ,INPUT-OUTPUT pcNotValidFields CHARACTER    ��  8�  D�      updateTitle ,   (�  X�  h�      updateRecord    ,   H�  |�  ��      updateMode  ,INPUT pcMode CHARACTER l�  ��  ��      showDataMessagesProcedure   ,OUTPUT pcReturn CHARACTER  ��  ��  �      resetRecord ,   ��  �  (�      queryPosition   ,INPUT pcState CHARACTER    �  T�  l�      okToContinueProcedure   ,INPUT pcAction CHARACTER,OUTPUT plAnswer LOGICAL   D�  ��  ��      deleteRecord    ,   ��  ��  ��      dataAvailable   ,INPUT pcRelative CHARACTER ��  �  �      confirmExit ,INPUT-OUTPUT plCancel LOGICAL   �  L�  \�      confirmDelete   ,INPUT-OUTPUT plAnswer LOGICAL  <�  ��  ��      confirmContinue ,INPUT-OUTPUT plCancel LOGICAL  |�  ��  ��      collectChanges  ,INPUT-OUTPUT pcChanges CHARACTER,INPUT-OUTPUT pcInfo CHARACTER ��  ,�  8�      viewRecord  ,   �  L�  \�      valueChanged    ,   <�  p�  |�      updateState ,INPUT pcState CHARACTER    `�  ��  ��      toolbar ,INPUT pcValue CHARACTER    ��  ��  ��      initializeObject    ,   ��  �  �      enableFields    ,   ��  (�  8�      displayFields   ,INPUT pcColValues CHARACTER    �  h�  x�      disableFields   ,INPUT pcFieldType CHARACTER    X�  ��  ��      copyRecord  ,   ��  ��  ��      cancelRecord    ,   ��  ��  ��      addRecord   ,        � 
"     
 �%     adecomm/as-utils.w 
"   
   �    }        �
"     
   %              %              %              %              %              %              %              %              %              %              %              %               �     }        �� �  O   %               � 
"    
 %              � �  �         `      $              
�    � �        
�             �G                      
�            � �   
"    
 �
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �            7%               
"   
 ��           8    1� �  
 �� �   %               o%   o           � �    �
"   
 ��           �    1� �   �� �   %               o%   o           � �   �
"   
 ��                1� �  
 �� �   %               o%   o           � �   �
"   
 ��           �    1�    �� �   %               o%   o           �    �
"   
 ��               1�    �� �   %               o%   o           � +   �
"   
 ��           |    1� B   �� N   %               o%   o           %               
"   
 �          �    1� V   � f     
"   
 ��           4    1� m   �� �   %               o%   o           � �  � �
"   
 ��           �    1� ?   �� �   %               o%   o           � N  N �
"   
 ��               1� �   �� N   %               o%   o           %               
"   
 ��           �    1� �   �� N   %               o%   o           %               
"   
 ��               1� �   �� N   %               o%   o           %              
"   
 �          �    1� �   � N     
"   
 ��           �    1� �  
 �� N   %               o%   o           %               
"   
 ��           H	    1� �   �� �   %               o%   o           � �    �
"   
 �          �	    1� �   � f     
"   
 ��           �	    1� �   �� �   %               o%   o           �   t �
"   
 �          l
    1� �  
 � f     
"   
 ��           �
    1� �   �� �   %               o%   o           � �  � �
"   
 ��               1� 2   �� �   %               o%   o           � �    �
"   
 ��           �    1� I  
 �� T   %               o%   o           %               
"   
 ��               1� X   �� N   %               o%   o           %               
"   
 ��           �    1� `   �� �   %               o%   o           � �    �
"   
 ��           �    1� q   �� �   %               o%   o           o%   o           
"   
 ��           x    1� �  
 �� �   %               o%   o           � �    �
"   
 ��           �    1� �   �� �  	 %               o%   o           � �  / �
"   
 �          `    1� �   � �  	   
"   
 ��           �    1� �   �� �  	 o%   o           o%   o           � �    �
"   
 �              1� �   � �  	   
"   
 ��           L    1�    �� �  	 o%   o           o%   o           � �    �
"   
 �          �    1�    � N     
"   
 �          �    1� )   � �  	   
"   
 �          8    1� 6   � �  	   
"   
 �          t    1� C   � �  	   
"   
 ��           �    1� Q   �� N   o%   o           o%   o           %              
"   
 �          ,    1� b   � �  	   
"   
 �          h    1� p  
 � {     
"   
 �          �    1� �   � �  	   
"   
 �          �    1� �   � �  	   
"   
 �              1� �   � �  	   
"   
 �          X    1� �   � �  	   
"   
 �          �    1� �  	 � �  	   
"   
 �          �    1� �   � �  	   
"   
 �              1� �   � �  	   
"   
 ��           H    1� �   �� �   %               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 7
"   
   
"   
 �(�  L ( l       �            �� 	   � P   �            �@    
� @  , 
�       (    ��      p�               �L
�    %              � 8      4    � $         �           
�    � 3     
"   
 �� @  , 
�       D    �� �  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 7�           �    1� 6  
 7� �   %               o%   o           � �    7
"   
 7�           d    1� A  
 7� �   %               o%   o           o%   o           
"   
 7�           �    1� L   7� f   %               o%   o           o%   o           
"   
 ��           \    1� U   �� N   %               o%   o           %               
"   
 ��           �    1� d   �� N   %               o%   o           %               
"   
 ��           T    1� q   �� �   %               o%   o           � �    �
"   
 ��           �    1� x   �� N   %               o%   o           %              
"   
 ��           D    1� �   �� N   %               o%   o           o%   o           
"   
 ��           �    1� �   �� �   %               o%   o           o%   o           
"   
 7�           <    1� �  	 7� �   %               o%   o           � �    �
"   
 7�           �    1� �   7� �   %               o%   o           o%   o           
"   
 7�           ,    1� �   7� �   %               o%   o           o%   o           
"   
 ��           �    1� �   �� N   %               o%   o           %               
"   
 ��           $    1� �   �� N   %               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 7�           �    1� �   7� �  	 %               o%   o           � �    7
"   
 ��           h    1� �   �� �  	 %               o%   o           � �    7
"   
 7�           �    1�    7� N   %               o%   o           %               
"   
 ��           X    1�    �� �  	 %               o%   o           � �    7
"   
 7�           �    1� %   7� �  	 %               o%   o           � �    �
"   
 ��           @    1� 3   �� N   %               o%   o           %               
"   
 ��           �    1� A   �� �  	 %               o%   o           � �    �
"   
 8�           0     1� P   8� �  	 %               o%   o           � �    �
"   
 7�           �     1� _   7� �  	 %               o%   o           � �    8
"   
 7�           !    1� m   7� �  	 %               o%   o           o%   o           
"   
 7�           �!    1� {   7� �  	 %               o%   o           � �    �
"   
 ��           "    1� �   �� �  	 %               o%   o           � �    7
"   
 7�           |"    1� �  	 7� {   %               o%   o           %               
"   
 ��           �"    1� �   �� {   %               o%   o           %               
"   
 ��           t#    1� �   �� N   %               o%   o           o%   o           
"   
 ��           �#    1� �   �� N   %               o%   o           o%   o           
"   
 7�           l$    1� �   7� N   %               o%   o           %               
"   
 ��           �$    1� �   �� N   %               o%   o           %               
"   
 7�           d%    1� �   7� N   %               o%   o           %               
"   
 ��           �%    1�     ��    %               o%   o           %       
       
"   
 ��           \&    1�    ��    %               o%   o           o%   o           
"   
 ��           �&    1�     ��    %               o%   o           %              
"   
 ��           T'    1� ,   ��    %               o%   o           o%   o           
"   
 ��           �'    1� 8   ��    %               o%   o           %              
"   
 ��           L(    1� E   ��    %               o%   o           o%   o           
"   
 ��           �(    1� R   ��    %               o%   o           %              
"   
 ��           D)    1� Z   ��    %               o%   o           o%   o           
"   
 ��           �)    1� b   �� �  	 %               o%   o           � �    8P �L 
�H T   %              �     }        �GG %              
"   
 7�           �*    1� t   7� T   %               o%   o           %               
"   
 7�           +    1� �   7� T   %               o%   o           o%   o           
"   
 ��           �+    1� �   �� �   %               o%   o           � �    �
"   
 ��           �+    1� �   �� �   %               o%   o           � �  - �
"   
 7�           h,    1� �   7� �   %               o%   o           � �    �
"   
 ��           �,    1� �   �� �   %               o%   o           �    7
"   
 �          P-    1� 2   � f     
"   
 7�           �-    1� C   7� �   %               o%   o           � �    7
"   
 �           .    1� O  
 � f     
"   
 �          <.    1� Z   � f     
"   
 ��           x.    1� g   �� �  	 %               o%   o           � �    �
"   
 ��           �.    1� t   �� �   %               o%   o           � �    �
"   
 ��           `/    1� �   �� f   %               o%   o           o%   o           
"   
 ��           �/    1� �   �� �   %               o%   o           � �  ! �
"   
 8�           P0    1� �   8� �   %               o%   o           � �    �
"   
 ��           �0    1� �   �� �   %               o%   o           � �   8
"   
 ��           81    1� �  	 �� T   %               o%   o           o%   o           
"   
 ��           �1    1� �   �� N   %               o%   o           %               
"   
 �          02    1�    � f     
"   
 ��           l2    1�    �� �   %               o%   o           � *   7
"   
 ��           �2    1� 9   �� �  	 %               o%   o           � �    �
"   
 ��           T3    1� F   �� �  	 %               o%   o           � �    �
"   
 �          �3    1� V   � f     
"   
 �          4    1� h   � �  	   
"   
 ��           @4    1� {   �� N   o%   o           o%   o           %               
"   
 �          �4    1� �   � N     
"   
 �          �4    1� �   � �  	   
"   
 �          45    1� �   � �  	   
"   
 �          p5    1� �   � �  	   
"   
 �          �5    1� �   � �  	   
"   
 �          �5    1� �   � �  	   
"   
 �          $6    1� �   � f     
"   
 ��           `6    1�    �� �   %               o%   o           � %  4 7
"   
 �          �6    1� Z   � f     
"   
 �          7    1� g   � f     
"   
 �          L7    1� w   � f     
"   
 �          �7    1� �   � �  	   
"   
 �          �7    1� �   � �  	   
"   
 �           8    1� �   � �  	   
"   
 �          <8    1� �   � N     
"   
 ��           x8    1� �   �� �  	 %               o%   o           � �    �
"   
 8�           �8    1� �   8� �  	 %               o%   o           � �    �
"   
 7�           `9    1� �   7� �  	 %               o%   o           � �    8
"   
 ��           �9    1� �   �� �  	 %               o%   o           � �    7
"   
 7�           H:    1�     7� N   %               o%   o           %               
"   
 7�           �:    1�     7� N   %               o%   o           o%   o           
"   
 ��           @;    1� -    �� N   %               o%   o           %               
"   
 ��           �;    1� =    �� N   %               o%   o           %               
"   
 ��           8<    1� I    �� N   %               o%   o           o%   o           
"   
 8�           �<    1� d    8� N   %               o%   o           %               
"   
 �          0=    1� r    � �  	   
"   
 7�           l=    1� �    7� N   %               o%   o           %              
"   
 �          �=    1� �    � �  	   
"   
 �          $>    1� �    � �  	   
"   
 �          `>    1� �   
 � �  	   
"   
 ��           �>    1� �    �� �  	 %               o%   o           �     �
"   
 7�           ?    1� �    7� �  	 %               o%   o           � �    �P �L 
�H T   %              �     }        �GG %              
"   
 8�           �?    1� �    8� �   %               o%   o           � �    8
"   
 ��           L@    1� �    �� N   %               o%   o           %               
"   
 ��           �@    1� �    �� �   %               o%   o           � �    �
"   
 ��     ,      <A    1� !   �� �   %               o%   o           �   � �     � !   ��    	 �
"   
 ��           �A    1� !   �� N   %               o%   o           o%   o           
"   
 ��           LB    1�  !   �� �   %               o%   o           � �    �
"   
 7�           �B    1� .!   7� �   %               o%   o           � �    �
"   
 7�           4C    1� =!   7� �  	 %               o%   o           o%   o           
"   
 7�           �C    1� U!   7� �   %               o%   o           o%   o           
"   
 ��           ,D    1� d!   �� �   %               o%   o           � �    8
"   
 7�           �D    1� q!   7� N   %               o%   o           %               
"   
 �          E    1� !   � f     
"   
 ��           XE    1� �!   �� �   %               o%   o           � �!  ~ �
"   
 7�           �E    1� ("   7� �   %               o%   o           � �    �
"   
 ��           @F    1� :"   �� �   %               o%   o           � R"   7
"   
 7�           �F    1� h"   7� �  	 %               o%   o           � �"   �
"   
 8�           (G    1� �"   8� �  	 %               o%   o           � �"   7
"   
 ��           �G    1� �"  	 �� �   %               o%   o           � �"   8
"   
 7�           H    1� �"  
 7� �  	 %               o%   o           � �"   �
"   
 7�           �H    1� �"   7� N   %               o%   o           o%   o           
"   
 ��            I    1� �"   �� �   %               o%   o           � �"   �
"   
 7�           tI    1� �"   7� �   %               o%   o           � �    �
"   
 7�           �I    1� �"  
 7� N   %               o%   o           o%   o           
"   
 �          dJ    1� �"   � f     
"   
 8�           �J    1� #   8� �   %               o%   o           � !#  ] 7
"   
 ��           K    1� #   �� �   %               o%   o           � �    8
"   
 7�           �K    1� �#   7� �   %               o%   o           � �#   �
"   
 ��           �K    1� �#   �� N   %               o%   o           %               
"   
 7�           xL    1� t   7� �   %               o%   o           � �    �
"   
 7�           �L    1� �#   7� �   %               o%   o           o%   o           
"   
 �          hM    1� �#   � �  	   P �L 
�H T   %              �     }        �GG %              
"   
 7�           �M    1� �#   7� N   %               o%   o           %               
"   
 8�           tN    1� �#  	 8� N   %               o%   o           %               
"   
 �          �N    1� �#   � �         
�    
�        �     }         �     }        �     }             �     }        �%                  �     }         �     }        �     }             �     }        �%              
�             �G "    %     start-super-proc �%     adm2/smart.p �P �L 
�H T   %              �     }        �GG %              
"   
   �       �P    6� 	     
"   
   
�        Q    8
"   
   �        ,Q    ��     }        �G 4              
"   
 ߱G %              G %              %� � �   EnabledObjFldsToDisable,ModifyFields,DataSourceNames,UpdateTargetNames,LogicalObjectName,LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout 
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
   (�  L ( l       �        �R    �� 	   � P   �        �R    �@    
� @  , 
�       �R    ��    �p�               �L
�    %              � 8      �R    � $         �           
�    � 3   �
"   
 �p� @  , 
�        T    �� m   �p�               �L"    , �   � ,$   �� .$   �     }        �A      |    "      � ,$   �%              (<   \ (    |    �     }        �A� 0$   �A"  	  �    "    �"  	  �  < "    �"  	  �(    |    �     }        �A� 0$   �A"  	  �
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
   (�  L ( l       �        �U    �� 	   � P   �        �U    �@    
� @  , 
�       �U    ��    �p�               �L
�    %              � 8      �U    � $         �           
�    � 3   �
"   
 �p� @  , 
�       W    �� �  
 �p�               �L"    , 
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
 �(�  L ( l       �        �W    �� 	   � P   �        �W    �@    
� @  , 
�       �W    ��    �p�               �L
�    %              � 8      �W    � $         �    �     
�    � 3   
"   
 �p� @  , 
�       �X    �� V   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
   
"   
 
"   
   
"   
   (�  L ( l       �        �Y    �� 	   � P   �        �Y    �@    
� @  , 
�       �Y    ��      p�               �L
�    %              � 8      �Y    � $         �           
�    � 3     
"   
 �p� @  , 
�       �Z    �� �  
 �p�               �L%     SmartDataViewer 
"   
   p� @  , 
�       ([    ��      p�               �L%      FRAME   
"   
  p� @  , 
�       �[    ��     p�               �L%               
"   
  p� @  , 
�       �[    �� �    p�               �L(        � �      � �      � �      �     }        �A
�H T   %              �     }        �GG %              
"   
 7 (   � 
"   
 �    �        �\    �� 	   �
"   
   � 8      ]    � $         �           
�    � 3   �
"   
   �        l]    �
"   
   �       �]    /
"   
   
"   
   �       �]    6� 	     
"   
   
�        �]    8
"   
   �        ^    �
"   
   �       $^    �
"   
   p�    � Y$   �
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 �    �        �^    �A"    �A
"   
   
�        4_    �@ � 
"   
 7"      �       }        �
"   
 %              %                "    %     start-super-proc �%     adm2/appserver.p c��    � �$     
�    �     }        �%               %      Server  - �     }        �    "  
  7� �    %                   "    7� �    %      NONE    p�,  8         $     "    �        � �$   �
�    
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
   (�  L ( l       �        ta    �� 	   � P   �        �a    �@    
� @  , 
�       �a    ��    �p�               �L
�    %              � 8      �a    � $         �           
�    � 3   �
"   
 �p� @  , 
�       �b    �� �   �p�               �L"    , p�,  8         $     "  
  �        � %   �
�     "    %     start-super-proc �%     adm2/visual.p ��   � �     � !     � o     
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
   (�  L ( l       �        d    �� 	   � P   �        d    �@    
� @  , 
�       d    ��    �p�               �L
�    %              � 8      (d    � $         �           
�    � 3   �
"   
 �p� @  , 
�       8e    �� A   �p�               �L"    , � 
"    
 %     contextHelp 
"    
   
�    
�    %     processAction   
�    %     CTRL-PAGE-UP �%     processAction   
�    %     CTRL-PAGE-DOWN  "    %     start-super-proc �%     adm2/containr.p %     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents �%      initializeDataObjects �0 0   A    �    � l%   �
�    � ~%   A    �    � l%     
�    � �%   %     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents %     buildDataRequest ent0 A    �    � l%   
�    � �%   7%     modifyListProperty 
�    
�    %      Add     %     SupportedLinks %      ContainerToolbar-Target %               
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
 8(�  L ( l       �        4i    �� 	   � P   �        @i    �@    
� @  , 
�       Li    ��    �p�               �L
�    %              � 8      Xi    � $         �    �     
�    � 3   
"   
 �p� @  , 
�       hj    �� V   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
 �
"   
 
"   
 �
"   
 �(�  L ( l       �        k    �� 	   � P   �         k    �@    
� @  , 
�       ,k    ��    �p�               �L
�    %              � 8      8k    � $         �    �     
�    � 3   �
"   
 �p� @  , 
�       Hl    ��     �p�               �L%               "    %     start-super-proc �%     adm2/datavis.p %     modifyListProperty 
�    %      ADD     %     SupportedLinks %     Toolbar-Target %     valueChanged    
�    %     valueChanged    
�     "    %     start-super-proc �%     adm2/viewer.p �%     modifyListProperty 
�    
�    %      Add     %     DataSourceEvents �7%     buildDataRequest �7�   � �   �� !     � >&  { ��   � �     � !   �� �&  B ��@    �    � �   �� �&   �     � �   �"    �� �   �@    �    � �     � �&         � �   �"    � �     
�H T   %              �     }        �GG %              
"   
 
"   
 �
"   
 
"   
 (�  L ( l       �        �o    �� 	   � P   �        �o    �@    
� @  , 
�       �o    ��    p�               �L
�    %              � 8      �o    � $         �         
�    � 3     
"   
 �p� @  , 
�        q    �� �    �p�               �L"    , 
"   
   p� @  , 
�       Xq    ��  !     p�               �L"    , 
"   
  p� @  , 
�       �q    �� �"  
  p�               �L%               �             I%               �             �%              �     }        �
�                    �           �   l       ��                 W  {  �               ��7                    O   ����    e�          O   ����    R�          O   ����    ��        $  f  �   ���                       |\     
                    � ߱              g  (  �      �\      4   �����\                �                      ��                  h  z                  �^                       h  8  �  �  i   ]            k  �  `      x]      4   ����x]                p                      ��                  l  y                  \_                       l  �  �  o   m      ,                                 �  �   n  �]      �  �   o  �]      $  $  p  �  ���                       �]     
                    � ߱        8  �   q  ^      L  �   r  0^      `  �   u  P^          $   x  �  ���                       �^  @         l^              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �  �  �               �`                    O   ����    e�          O   ����    R�          O   ����    ��      z$                      �          �  $  �    ���                       �^     
                    � ߱                  �  �                      ��                   �  �                  �f                     �  4      4   �����^      $  �  �  ���                       @_     
                    � ߱        �    �  4  D      T_      4   ����T_      /  �  p                               3   ����h_  �  �   �  t_          O   �  ��  ��  �_                               , �                          
                               �      ��                            ����                                                        �   l       ��                  ,  M  �               l�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                                            �           �   l       ��                  �  �  �               �1                    O   ����    e�          O   ����    R�          O   ����    ��      �      �  �� �                       �  �         `r      4   ����`r      �   �  tr    ��                              ��        v                  ����                               �    d d     �   ��M6  M6  � �                                               v      �                                                                 d     D                                                                 P    �Q                                                           '  G   
 X   Q          �   �                                         �     }      P   p �Q                                                           ('  G   
 X  p Q         �   �                                         �     }      P   � WQ                                                           /'  G   
 X  � Q         �   �                                         �     }      P   t' uQ                                                           3'  G   
 X  t' �Q                                                   �      �      P   81 1Q                                                           <'  G   
 X  81 Q         ,  @                              
          �     �      H  d d L6�          d   x                   o                     D                                                                    TXS appSrvUtils RowObject CodAnt CodCli CodCta CodMon CodRef DirCli FchAnu FchDoc FchVto FmaPgo Glosa NomCli NroDoc NroOrd NroPed NroRef RucCli UsuAnu usuario CodCia CodDiv CodDoc FlgEst PorIgv PorDto TpoCmb Cndcre TpoFac Tipo SdoAct CodAlm CodMov CodVen ImpTot CCo ImpIgv ImpIsc ImpBrt ImpDto ImpDto2 ImpExo ImpVta imptot2 Libre_d02 Dcto_Otros_VV AcuBon1 AcuBon2 AcuBon3 AcuBon4 AcuBon5 AcuBon6 AcuBon7 AcuBon8 AcuBon9 AcuBon10 TotalVenta TotalIGV TotalMontoICBPER TotalValorVentaNetoOpGravadas ASSIGNFOCUSEDWIDGET ASSIGNWIDGETVALUE ASSIGNWIDGETVALUELIST BLANKWIDGET CLEARWIDGET DISABLERADIOBUTTON DISABLEWIDGET ENABLERADIOBUTTON ENABLEWIDGET FORMATTEDWIDGETVALUE FORMATTEDWIDGETVALUELIST HIDEWIDGET HIGHLIGHTWIDGET RESETWIDGETVALUE TOGGLEWIDGET VIEWWIDGET WIDGETHANDLE WIDGETLONGCHARVALUE WIDGETISBLANK WIDGETISFOCUSED WIDGETISMODIFIED WIDGETISTRUE WIDGETVALUE WIDGETVALUELIST RECT-1 F-Main >>>>>>>>>>>9.99 ->>9.99 -ZZ,ZZZ,ZZ9.99 D:\newsie\on_in_co\aplic\CCB\vnotacrdb-totales.w should only be RUN PERSISTENT. GETTARGETPROCEDURE GETOBJECTTYPE GETSHOWPOPUP SETSHOWPOPUP GETCREATEHANDLES GETDATAMODIFIED GETDISPLAYEDFIELDS GETDISPLAYEDTABLES GETENABLEDFIELDS GETENABLEDHANDLES GETFIELDHANDLES GETFIELDSENABLED GETGROUPASSIGNSOURCE GETGROUPASSIGNSOURCEEVENTS GETGROUPASSIGNTARGET GETGROUPASSIGNTARGETEVENTS GETNEWRECORD GETOBJECTPARENT GETRECORDSTATE GETROWIDENT GETTABLEIOSOURCE GETTABLEIOSOURCEEVENTS GETUPDATETARGET GETUPDATETARGETNAMES GETWINDOWTITLEFIELD OKTOCONTINUE SETCONTAINERMODE SETDATAMODIFIED SETDISPLAYEDFIELDS SETENABLEDFIELDS SETGROUPASSIGNSOURCE SETGROUPASSIGNSOURCEEVENTS SETGROUPASSIGNTARGET SETGROUPASSIGNTARGETEVENTS SETLOGICALOBJECTNAME SETOBJECTPARENT SETSAVESOURCE SETTABLEIOSOURCE SETTABLEIOSOURCEEVENTS SETUPDATETARGET SETUPDATETARGETNAMES SETWINDOWTITLEFIELD SHOWDATAMESSAGES DISABLEPAGESINFOLDER ENABLEPAGESINFOLDER GETCALLERPROCEDURE GETCALLERWINDOW GETCONTAINERMODE GETCONTAINERTARGET GETCONTAINERTARGETEVENTS GETCURRENTPAGE GETDISABLEDADDMODETABS GETDYNAMICSDOPROCEDURE GETFILTERSOURCE GETMULTIINSTANCEACTIVATED GETMULTIINSTANCESUPPORTED GETNAVIGATIONSOURCE GETNAVIGATIONSOURCEEVENTS GETNAVIGATIONTARGET GETOUTMESSAGETARGET GETPAGENTARGET GETPAGESOURCE GETPRIMARYSDOTARGET GETREENABLEDATALINKS GETRUNDOOPTIONS GETRUNMULTIPLE GETSAVEDCONTAINERMODE GETSDOFOREIGNFIELDS GETTOPONLY GETUPDATESOURCE GETWAITFOROBJECT GETWINDOWTITLEVIEWER GETSTATUSAREA PAGENTARGETS SETCALLEROBJECT SETCALLERPROCEDURE SETCALLERWINDOW SETCONTAINERTARGET SETCURRENTPAGE SETDISABLEDADDMODETABS SETDYNAMICSDOPROCEDURE SETFILTERSOURCE SETINMESSAGETARGET SETMULTIINSTANCEACTIVATED SETMULTIINSTANCESUPPORTED SETNAVIGATIONSOURCE SETNAVIGATIONSOURCEEVENTS SETNAVIGATIONTARGET SETOUTMESSAGETARGET SETPAGENTARGET SETPAGESOURCE SETPRIMARYSDOTARGET SETREENABLEDATALINKS SETROUTERTARGET SETRUNDOOPTIONS SETRUNMULTIPLE SETSAVEDCONTAINERMODE SETSDOFOREIGNFIELDS SETTOPONLY SETUPDATESOURCE SETWAITFOROBJECT SETWINDOWTITLEVIEWER SETSTATUSAREA GETALLFIELDHANDLES GETALLFIELDNAMES GETCOL GETDEFAULTLAYOUT GETDISABLEONINIT GETENABLEDOBJFLDS GETENABLEDOBJHDLS GETHEIGHT GETHIDEONINIT GETLAYOUTOPTIONS GETLAYOUTVARIABLE GETOBJECTENABLED GETOBJECTLAYOUT GETROW GETWIDTH GETRESIZEHORIZONTAL GETRESIZEVERTICAL SETALLFIELDHANDLES SETALLFIELDNAMES SETDEFAULTLAYOUT SETDISABLEONINIT SETHIDEONINIT SETLAYOUTOPTIONS SETOBJECTLAYOUT SETRESIZEHORIZONTAL SETRESIZEVERTICAL GETOBJECTTRANSLATED GETOBJECTSECURED CREATEUIEVENTS GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALVERSION SETOBJECTNAME SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE , prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDataViewer ContainerType FRAME PropertyDialog adm2/support/viewerd.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties EnabledObjFldsToDisable,ModifyFields,DataSourceNames,UpdateTargetNames,LogicalObjectName,LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout SupportedLinks Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext ObjectLayout LayoutOptions ObjectEnabled LayoutVariable DefaultLayout DisableOnInit EnabledObjFlds EnabledObjHdls FieldSecurity SecuredTokens AllFieldHandles AllFieldNames MinHeight MinWidth ResizeHorizontal ResizeVertical ObjectSecured ObjectTranslated PopupButtonsInFields ColorInfoBG INTEGER ColorInfoFG ColorWarnBG ColorWarnFG ColorErrorBG ColorErrorFG BGColor FGColor FieldPopupMapping CurrentPage PendingPage ContainerTarget ContainerTargetEvents exitObject,okObject,cancelObject,updateActive ContainerToolbarSource ContainerToolbarSourceEvents toolbar,okObject,cancelObject OutMessageTarget PageNTarget PageSource FilterSource UpdateSource UpdateTarget CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState StartPage RunMultiple WaitForObject DynamicSDOProcedure adm2/dyndata.w RunDOOptions InitialPageList WindowFrameHandle Page0LayoutManager MultiInstanceSupported MultiInstanceActivated ContainerMode SavedContainerMode SdoForeignFields NavigationSource NavigationTarget PrimarySdoTarget NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter CallerWindow CallerProcedure CallerObject DisabledAddModeTabs ReEnableDataLinks WindowTitleViewer UpdateActive InstanceNames ClientNames ContainedDataObjects ContainedAppServices DataContainer HasDbAwareObjects HasDynamicProxy HideOnClose HideChildContainersOnClose HasObjectMenu RequiredPages RemoveMenuOnHide ProcessList PageLayoutInfo PageTokens DataContainerName WidgetIDFileName CreateHandles DataModified DisplayedFields DisplayedTables   Editable EnabledFields EnabledHandles EnabledObjFldsToDisable EnabledWhenNew FieldHandles FieldsEnabled GroupAssignSource GroupAssignSourceEvents addRecord,copyRecord,updateRecord,resetRecord,undoRecord,cancelRecord,enableFields,disableFields,collectChanges,validateFields GroupAssignTarget GroupAssignTargetEvents updateState,LinkState InternalDisplayFromSource (Large) ModifyFields (All) NewRecord No ObjectMode View PrintPreviewActive RecordState NoRecordAvailable RowIdent SaveSource TableIOSource TableIOSourceEvents addRecord,updateRecord,copyRecord,deleteRecord,resetRecord,undoChange,cancelRecord,updateMode ToolbarSource ToolbarSourceEvents toolbar UndoNew UpdateTargetNames WindowTitleField KeepChildPositions ShowPopup FieldWidgetIDs ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService cFields adm2/visual.p CTRL-PAGE-UP CTRL-PAGE-DOWN adm2/containr.p Add initializeDataObjects getSupportedLinks data-target data-source buildDataRequest containertoolbar-target ContainerToolbar-Target adm2/datavis.p ADD Toolbar-Target DISPLAYOBJECTS cViewCols cEnabled iCol iEntries cEntry adm2/viewer.p RowObject.TotalValorVentaNetoOpGravadas RowObject.TotalMontoICBPER RowObject.TotalIGV RowObject.PorIgv RowObject.TotalVenta RowObject.TotalMontoICBPER RowObject.TotalIGV RowObject.TotalVenta ,RowObject. DISABLE_UI default VALOR VENTA ICBPER IGV % I.G.V. IMPORTE TOTAL �  �&  �  �/      3 �    ��      0         pcFieldType     ��      T         pcColValues     ��      x         pcValue     ��      �         pcState �   ��      �         pcChanges       ��      �         pcChanges       ��               plCancel        ��      $        plAnswer        ��      H        plCancel        ��      l        pcRelative  �  ��      �        pcAction        ��      �        pcAction        ��      �        pcState     ��      �        pcReturn        ��              pcMode      ��      <        pcState     ��      \        pcNotValidFields    �  ��      �        pcProp      ��      �        pcProp      ��      �        plCancel    �  ��      �        pcProcName    ��             
 pcProcName  @  ��      4        pcProcName      ��      X       
 pcProcName      ��      |        piPageNum       ��      �        piPageNum       ��      �        pcPageList      ��      �        pcProc    ��              pcLinkName      ��      ,        pcLinkName  \  ��      P       
 phTarget        ��      t        phTarget        ��      �        piPageNum       ��      �        pcValue     ��      �        piPageNum       ��               pcAction        ��      $       
 phAppService        ��      L        pcMode  x  ��      l       
 phSource    �  ��      �        phSource        ��      �       
 phSource    �  ��      �        pcText     ��      �        pcText      ��              pcText  D  ��      8       
 phObject    h  ��      \       
 phObject        ��      �        phObject        ��      �        pcField     ��      �        pcCursor    �  ��      �       
 phCaller      ��              phCaller    <  ��      0        phCaller        ��      T        phCaller    �  ��      x        pcMod   �  ��      �        pcMod       ��      �       
 pcMod   �  ��      �       
 phSource      ��      �        phSource        ��              
 phSource    L  ��      D        pdRow       ��      d        pdRow       ��      �       
 hTarget �  ��      �        pcMessage       ��      �        pcMessage       ��      �        plEnabled             	     cType       T	     T   �          D	                  getObjectType   g    �  �	        t	  
   hReposBuffer    �	        �	  
   hPropTable  �	        �	  
   hBuffer           �	  
   hTable  	  
     U   `	          
                  adm-clone-props f  g  h  i  k  l  m  n  o  p  q  r  u  x  y  z  {            t
  
   hProc             �
        pcProcName  �	  �
  	   V   `
  |
      �
                  start-super-proc    �  �  �  �  �  �  �  �  �  �
  8     W                                   �    l     X                                   �  �  <  �     Y                                   �  �  t  �     Z               �                  displayObjects  M  �        [                                   j  �  T     \                                   p  $  �     ]               �                  disable_UI  �  �  �  X  �  &    
 �      �                          �  �  ;   RowObject   �         �         �         �         �         �         �         �         �         �                                                        (         0         8         @         H         P         X         `         h         p         x         �         �         �         �         �         �         �         �         �         �         �         �         �         �         �         �         �                                     (         0         8         @         H         P         X         `         h         t         �         �         �         CodAnt  CodCli  CodCta  CodMon  CodRef  DirCli  FchAnu  FchDoc  FchVto  FmaPgo  Glosa   NomCli  NroDoc  NroOrd  NroPed  NroRef  RucCli  UsuAnu  usuario CodCia  CodDiv  CodDoc  FlgEst  PorIgv  PorDto  TpoCmb  Cndcre  TpoFac  Tipo    SdoAct  CodAlm  CodMov  CodVen  ImpTot  CCo ImpIgv  ImpIsc  ImpBrt  ImpDto  ImpDto2 ImpExo  ImpVta  imptot2 Libre_d02   Dcto_Otros_VV   AcuBon1 AcuBon2 AcuBon3 AcuBon4 AcuBon5 AcuBon6 AcuBon7 AcuBon8 AcuBon9 AcuBon10    TotalVenta  TotalIGV    TotalMontoICBPER    TotalValorVentaNetoOpGravadas   �          �  
   appSrvUtils         �  
   gshAstraAppserver   0          
   gshSessionManager   T        D  
   gshRIManager    |        h  
   gshSecurityManager  �        �  
   gshProfileManager   �        �  
   gshRepositoryManager    �  	 	     �  
   gshTranslationManager      
 
       
   gshWebManager   D        4     gscSessionId    h        X     gsdSessionObj   �        |  
   gshFinManager   �        �  
   gshGenManager   �        �  
   gshAgnManager   �        �     gsdTempUniqueID              gsdUserObj  @        ,     gsdRenderTypeObj    h        T     gsdSessionScopeObj  �       |  
   ghProp  �       �  
   ghADMProps  �       �  
   ghADMPropsBuf   �       �     glADMLoadFromRepos              glADMOk ,          
   ghContainer L       @     cObjectName h    	   `     iStart  �    
   |     cAppService �       �     cASDivision �       �     cServerOperatingMode    �       �     cFields             cViewCols   0       $     cEnabled    L       D     iCol    l       `     iEntries             �     cEntry        X  �  RowObject            |   �  �  �  �  5
  6
  7
  8
  O
  [
  \
  ]
  _
  a
  b
  c
  g
  h
  k
  l
  m
  n
  p
  r
  t
  v
  w
  x
  {
  }
  ~
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
  �
  �
  �
  �
  �
  �
  �
  �
  ,  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  (  3  4  6  7  8  9  :  ;  <  =  >  ?  @  A  B  C  D  E  F  G  H  I  J  K  L  M  N  O  P  Q  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �                     	  
            �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  )  4  5  7  8  9  �  n  o  q  r  u  v  x  �  �  �  �  �  �  �  �  �  �  �  �    ,  H  J  _  �  �       *  +  ,  /  0  1  8  9  V  j  �       $  .  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  f  �  �  �  �  �  �  i  o  s  t  u  v  {  �  �  �      :%  C:\Progress\OpenEdge\src\adm2\viewer.i   <  �Q 2 %C:\Progress\OpenEdge\src\adm2\custom\viewercustom.i  p  } & C:\Progress\OpenEdge\src\adm2\datavis.i  �  � 1 %C:\Progress\OpenEdge\src\adm2\custom\dataviscustom.i �  f! ' C:\Progress\OpenEdge\src\adm2\containr.i $  � 0 %C:\Progress\OpenEdge\src\adm2\custom\containrcustom.i    X  �� ( C:\Progress\OpenEdge\src\adm2\visual.i   �  # / %C:\Progress\OpenEdge\src\adm2\custom\visualcustom.i  �  �< ) C:\Progress\OpenEdge\src\adm2\appserver.i      �� . %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   H  I� * C:\Progress\OpenEdge\src\adm2\smart.i    �  Ds - C:\Progress\OpenEdge\gui\fn  �  tw , %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   �  Q. + C:\Progress\OpenEdge\gui\set (  �/  C:\Progress\OpenEdge\src\adm2\viewprop.i P  �� $ %C:\Progress\OpenEdge\src\adm2\custom\viewpropcustom.i    �  ۃ % %C:\Progress\OpenEdge\src\adm2\custom\viewprtocustom.i    �  ��  C:\Progress\OpenEdge\src\adm2\dvisprop.i   B� " %C:\Progress\OpenEdge\src\adm2\custom\dvispropcustom.i    @  �� # %C:\Progress\OpenEdge\src\adm2\custom\dvisprtocustom.i    �  ��  C:\Progress\OpenEdge\src\adm2\cntnprop.i �  ��   %C:\Progress\OpenEdge\src\adm2\custom\cntnpropcustom.i    �  P ! %C:\Progress\OpenEdge\src\adm2\custom\cntnprtocustom.i    @   F>  C:\Progress\OpenEdge\src\adm2\visprop.i  �   �I  %C:\Progress\OpenEdge\src\adm2\custom\vispropcustom.i �   ��  %C:\Progress\OpenEdge\src\adm2\custom\visprtocustom.i �   �l  C:\Progress\OpenEdge\src\adm2\appsprop.i 8!  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    l!  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    �!  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i �!  �j  C:\Progress\OpenEdge\gui\get ("  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    P"  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    �"  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i �"  Su  C:\Progress\OpenEdge\src\adm2\globals.i  #  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i @#  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   �#  �  C:\Progress\OpenEdge\src\adm2\appsprto.i �#  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   �#  �X  C:\Progress\OpenEdge\src\adm2\visprto.i  @$  !�  %C:\Progress\OpenEdge\src\adm2\custom\visualdefscustom.i  t$  n�  C:\Progress\OpenEdge\src\adm2\cntnprto.i �$  ;  %C:\Progress\OpenEdge\src\adm2\custom\containrdefscustom.i    �$  �7 
 C:\Progress\OpenEdge\src\adm2\dvisprto.i 4%  0 	 %C:\Progress\OpenEdge\src\adm2\custom\datavisdefscustom.i h%  ��  C:\Progress\OpenEdge\src\adm2\viewprto.i �%  gf  %C:\Progress\OpenEdge\src\adm2\custom\viewerdefscustom.i  �%  ~�  C:\Progress\OpenEdge\src\adm2\widgetprto.i   $&  R�  D:\newsie\on_in_co\.\aplic\ccb\dccbcdocu.i   \&  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   �&  ��    D:\newsie\on_in_co\aplic\CCB\vnotacrdb-totales.w     �   v      '  �   Q     '     �  2   ('  �   �     8'     �  +   H'  �   �     X'     �  +   h'  �   �     x'     �  +   �'  \   U     �'  o      &   �'     �  1   �'  U   �  &   �'  �   �  '   �'     �  +   �'  �   �  '   �'     ^  +   (  �   V  '   (     �  0   ((  �   �  '   8(     �  -   H(  �   �  '   X(     �  -   h(  �   �  '   x(     �  -   �(  r   �  '   �(  n   �  (   �(     M  /   �(  i   H  (   �(     &  +   �(  P     (   �(  �     )   �(     �  .   )  �   �  )   )     �  +   ()  �   �  )   8)     b  +   H)  �   `  )   X)     >  +   h)  g   $  )   x)          �)  O   �  )   �)  �   w  *   �)     u  -   �)  �   E  *   �)     �  ,   �)  �   �  *   �)     �  +   �)  �   �  *   *     �  +   *  �   �  *   (*     z  +   8*  �   y  *   H*     W  +   X*  �   F  *   h*     $  +   x*  �   !  *   �*     �  +   �*  }   �  *   �*     �  +   �*     U  *   �*       )   �*     �  (   �*     G  '   �*     �  &   +     �     +  u   �     (+  O   �  $   8+     �  %   H+     ?  $   X+  h   2     h+  �   )     x+  O     "   �+     
  #   �+     �  "   �+  {   �     �+  �   �     �+  O   r      �+     a  !   �+           �+  �   �     ,  �   �     ,  O   �     (,     �     8,     U     H,  �   0     X,  x   (     h,  M        x,          �,     �     �,  a   �     �,  �  ~     �,     _     �,  �  ,     �,  O        �,          �,     �
     -  �   �	     -     �     (-          8-  x   
     H-     �     X-     z     h-     v     x-     b     �-     I     �-  Q   9     �-     �     �-     �     �-     �     �-     y     �-  f   N     �-     �     .  "   �     .     �     (.     t     8.  Z   #     H.     +     X.     �     h.     �     x.     �     �.  X   �     �.     �  
   �.      �     �.     �  	   �.     z     �.  ]   o     �.     5     �.     �     /     �     /     �     (/     �     8/  0   $      H/     �      X/     b       h/     &      x/     !       �/           