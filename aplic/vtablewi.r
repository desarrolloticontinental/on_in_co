	��V�I ?    �              �                                 �� 3F000143utf-8 MAIN E:\OpenEdge\on_in_co\APLIC\vtablewi.w,, PROCEDURE disable_UI,, PROCEDURE displayObjects,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE startServerObject,, PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE restartServerObject,, PROCEDURE initializeServerObject,, PROCEDURE disconnectObject,, PROCEDURE destroyServerObject,, PROCEDURE bindServer,, PROCEDURE processAction,,INPUT pcAction CHARACTER PROCEDURE enableObject,, PROCEDURE disableObject,, PROCEDURE applyLayout,, PROCEDURE viewPage,,INPUT piPageNum INTEGER PROCEDURE viewObject,, PROCEDURE selectPage,,INPUT piPageNum INTEGER PROCEDURE removePageNTarget,,INPUT phTarget HANDLE,INPUT piPage INTEGER PROCEDURE passThrough,,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER PROCEDURE notifyPage,,INPUT pcProc CHARACTER PROCEDURE initPages,,INPUT pcPageList CHARACTER PROCEDURE initializeVisualContainer,, PROCEDURE hidePage,,INPUT piPageNum INTEGER PROCEDURE destroyObject,, PROCEDURE deletePage,,INPUT piPageNum INTEGER PROCEDURE createObjects,, PROCEDURE constructObject,,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE PROCEDURE changePage,, PROCEDURE assignPageProperty,,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER PROCEDURE validateFields,,INPUT-OUTPUT pcNotValidFields CHARACTER PROCEDURE updateTitle,, PROCEDURE updateRecord,, PROCEDURE updateMode,,INPUT pcMode CHARACTER PROCEDURE showDataMessagesProcedure,,OUTPUT pcReturn CHARACTER PROCEDURE resetRecord,, PROCEDURE queryPosition,,INPUT pcState CHARACTER PROCEDURE okToContinueProcedure,,INPUT pcAction CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE deleteRecord,, PROCEDURE dataAvailable,,INPUT pcRelative CHARACTER PROCEDURE confirmExit,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE confirmDelete,,INPUT-OUTPUT plAnswer LOGICAL PROCEDURE confirmContinue,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE collectChanges,,INPUT-OUTPUT pcChanges CHARACTER,INPUT-OUTPUT pcInfo CHARACTER PROCEDURE viewRecord,, PROCEDURE valueChanged,, PROCEDURE updateState,,INPUT pcState CHARACTER PROCEDURE toolbar,,INPUT pcValue CHARACTER PROCEDURE initializeObject,, PROCEDURE enableFields,, PROCEDURE displayFields,,INPUT pcColValues CHARACTER PROCEDURE disableFields,,INPUT pcFieldType CHARACTER PROCEDURE copyRecord,, PROCEDURE cancelRecord,, PROCEDURE addRecord,, FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION createUiEvents,LOGICAL, FUNCTION getObjectSecured,LOGICAL, FUNCTION getObjectTranslated,LOGICAL, FUNCTION setResizeVertical,LOGICAL,INPUT plResizeVertical LOGICAL FUNCTION setResizeHorizontal,LOGICAL,INPUT plResizeHorizontal LOGICAL FUNCTION setObjectLayout,LOGICAL,INPUT pcLayout CHARACTER FUNCTION setLayoutOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setHideOnInit,LOGICAL,INPUT plHide LOGICAL FUNCTION setDisableOnInit,LOGICAL,INPUT plDisable LOGICAL FUNCTION setDefaultLayout,LOGICAL,INPUT pcDefault CHARACTER FUNCTION setAllFieldNames,LOGICAL,INPUT pcValue CHARACTER FUNCTION setAllFieldHandles,LOGICAL,INPUT pcValue CHARACTER FUNCTION getResizeVertical,LOGICAL, FUNCTION getResizeHorizontal,LOGICAL, FUNCTION getWidth,DECIMAL, FUNCTION getRow,DECIMAL, FUNCTION getObjectLayout,CHARACTER, FUNCTION getObjectEnabled,LOGICAL, FUNCTION getLayoutVariable,CHARACTER, FUNCTION getLayoutOptions,CHARACTER, FUNCTION getHideOnInit,LOGICAL, FUNCTION getHeight,DECIMAL, FUNCTION getEnabledObjHdls,CHARACTER, FUNCTION getEnabledObjFlds,CHARACTER, FUNCTION getDisableOnInit,LOGICAL, FUNCTION getDefaultLayout,CHARACTER, FUNCTION getCol,DECIMAL, FUNCTION getAllFieldNames,CHARACTER, FUNCTION getAllFieldHandles,CHARACTER, FUNCTION setStatusArea,LOGICAL,INPUT plStatusArea LOGICAL FUNCTION setWindowTitleViewer,LOGICAL,INPUT phViewer HANDLE FUNCTION setWaitForObject,LOGICAL,INPUT phObject HANDLE FUNCTION setUpdateSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setTopOnly,LOGICAL,INPUT plTopOnly LOGICAL FUNCTION setSdoForeignFields,LOGICAL,INPUT cSdoForeignFields CHARACTER FUNCTION setSavedContainerMode,LOGICAL,INPUT cSavedContainerMode CHARACTER FUNCTION setRunMultiple,LOGICAL,INPUT plMultiple LOGICAL FUNCTION setRunDOOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setRouterTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setReEnableDataLinks,LOGICAL,INPUT cReEnableDataLinks CHARACTER FUNCTION setPrimarySdoTarget,LOGICAL,INPUT hPrimarySdoTarget HANDLE FUNCTION setPageSource,LOGICAL,INPUT phObject HANDLE FUNCTION setPageNTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setOutMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setNavigationTarget,LOGICAL,INPUT cTarget CHARACTER FUNCTION setNavigationSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setNavigationSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setMultiInstanceSupported,LOGICAL,INPUT lMultiInstanceSupported LOGICAL FUNCTION setMultiInstanceActivated,LOGICAL,INPUT lMultiInstanceActivated LOGICAL FUNCTION setInMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setFilterSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDynamicSDOProcedure,LOGICAL,INPUT pcProc CHARACTER FUNCTION setDisabledAddModeTabs,LOGICAL,INPUT cDisabledAddModeTabs CHARACTER FUNCTION setCurrentPage,LOGICAL,INPUT iPage INTEGER FUNCTION setContainerTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setCallerWindow,LOGICAL,INPUT h HANDLE FUNCTION setCallerProcedure,LOGICAL,INPUT h HANDLE FUNCTION setCallerObject,LOGICAL,INPUT h HANDLE FUNCTION pageNTargets,CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER FUNCTION getStatusArea,LOGICAL, FUNCTION getWindowTitleViewer,HANDLE, FUNCTION getWaitForObject,HANDLE, FUNCTION getUpdateSource,CHARACTER, FUNCTION getTopOnly,LOGICAL, FUNCTION getSdoForeignFields,CHARACTER, FUNCTION getSavedContainerMode,CHARACTER, FUNCTION getRunMultiple,LOGICAL, FUNCTION getRunDOOptions,CHARACTER, FUNCTION getReEnableDataLinks,CHARACTER, FUNCTION getPrimarySdoTarget,HANDLE, FUNCTION getPageSource,HANDLE, FUNCTION getPageNTarget,CHARACTER, FUNCTION getOutMessageTarget,HANDLE, FUNCTION getNavigationTarget,HANDLE, FUNCTION getNavigationSourceEvents,CHARACTER, FUNCTION getNavigationSource,CHARACTER, FUNCTION getMultiInstanceSupported,LOGICAL, FUNCTION getMultiInstanceActivated,LOGICAL, FUNCTION getFilterSource,HANDLE, FUNCTION getDynamicSDOProcedure,CHARACTER, FUNCTION getDisabledAddModeTabs,CHARACTER, FUNCTION getCurrentPage,INTEGER, FUNCTION getContainerTargetEvents,CHARACTER, FUNCTION getContainerTarget,CHARACTER, FUNCTION getContainerMode,CHARACTER, FUNCTION getCallerWindow,HANDLE, FUNCTION getCallerProcedure,HANDLE, FUNCTION enablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION disablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION showDataMessages,CHARACTER, FUNCTION setWindowTitleField,LOGICAL,INPUT cWindowTitleField CHARACTER FUNCTION setUpdateTargetNames,LOGICAL,INPUT pcTargetNames CHARACTER FUNCTION setUpdateTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setTableIOSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setTableIOSource,LOGICAL,INPUT phObject HANDLE FUNCTION setSaveSource,LOGICAL,INPUT plSave LOGICAL FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setLogicalObjectName,LOGICAL,INPUT pcLogicalObjectName CHARACTER FUNCTION setGroupAssignTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setGroupAssignTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setGroupAssignSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setGroupAssignSource,LOGICAL,INPUT phObject HANDLE FUNCTION setEnabledFields,LOGICAL,INPUT pcFieldList CHARACTER FUNCTION setDisplayedFields,LOGICAL,INPUT pcFieldList CHARACTER FUNCTION setDataModified,LOGICAL,INPUT plModified LOGICAL FUNCTION setContainerMode,LOGICAL,INPUT pcContainerMode CHARACTER FUNCTION okToContinue,LOGICAL,INPUT pcAction CHARACTER FUNCTION getWindowTitleField,CHARACTER, FUNCTION getUpdateTargetNames,CHARACTER, FUNCTION getUpdateTarget,CHARACTER, FUNCTION getTableIOSourceEvents,CHARACTER, FUNCTION getTableIOSource,HANDLE, FUNCTION getRowIdent,CHARACTER, FUNCTION getRecordState,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getNewRecord,CHARACTER, FUNCTION getGroupAssignTargetEvents,CHARACTER, FUNCTION getGroupAssignTarget,CHARACTER, FUNCTION getGroupAssignSourceEvents,CHARACTER, FUNCTION getGroupAssignSource,HANDLE, FUNCTION getFieldsEnabled,LOGICAL, FUNCTION getFieldHandles,CHARACTER, FUNCTION getEnabledHandles,CHARACTER, FUNCTION getEnabledFields,CHARACTER, FUNCTION getDisplayedTables,CHARACTER, FUNCTION getDisplayedFields,CHARACTER, FUNCTION getDataModified,LOGICAL, FUNCTION getCreateHandles,CHARACTER, FUNCTION setShowPopup,LOGICAL,INPUT plShowPopup LOGICAL FUNCTION getShowPopup,LOGICAL, FUNCTION getObjectType,character, FUNCTION getTargetProcedure,HANDLE, FUNCTION widgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION widgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION widgetIsTrue,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsModified,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetIsFocused,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsBlank,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetLongcharValue,LONGCHAR,INPUT pcName CHARACTER FUNCTION widgetHandle,HANDLE,INPUT pcName CHARACTER FUNCTION viewWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION toggleWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION resetWidgetValue,LOGICAL,INPUT pcNameList CHARACTER FUNCTION highlightWidget,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER FUNCTION hideWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION formattedWidgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION formattedWidgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION enableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION enableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION disableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION disableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION clearWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION blankWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION assignWidgetValueList,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION assignWidgetValue,LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER FUNCTION assignFocusedWidget,LOGICAL,INPUT pcName CHARACTER                     �             ��   ��              �o              �*    +   ,s t  U   �w D  V   �z �   Z   �{ d  ]           0} �  ? � �$  iSO8859-1                                                                           t    �                                      �                   ��                    p     �   m�   �             x�  �   �      �                                                         PROGRESS                                    
    
                    �              �                                                                                                     
  4         �          X  $     <     h�      h                       x          �      �   �  �      ,  
    
                    �             �                                                                                          �          
  \  �      �  
    
                  �  �             H                                                                                          �          
    �      �  
    
                  p  8             �                                                                                          �          
  �  �      0  
    
                    �             �                                                                                          �          
  `  �      �  
    
                  �  �             L                                                                                          �          
          �  
    
                  t  <             �                                                                                                    
  �        4  
    
                     �  	           �                                                                                                    
  d  1      �  
    
                  �  �  
           P                                                                                          1          
    ?      �                         x  @             �                                                                                          ?            �  L      8                        $  �             �                                                                                          L            h	  Z      �  
    
                  �  �	             T	                                                                                          Z          
  
  h      �	  
    
                  |	  D
              
                                                                                          h          
  �
  v      <
  
    
                  (
  �
             �
                                                                                          v          
  l  �      �
                        �
  �             X                                                                                          �              �      �                        �  H                                                                                                       �            �  �      @                        ,  �             �                                                                                          �                �      �                        �                 \                                                                                          �                          ��                                               ��          �  0  H X�            
             
             
                                         
                                                                                                                                                                        H   X   h   x   �   �   �   �   �   �   �   �       (  8  H      H   X   h   x   �   �   �   �   �   �   �   �      (  8  H                                                                            x  �  �  �  �          �             �  �  �  �  �          �             �  �  �    �                                                                   CodCia  999 Cia Cia 0   C�digo de Compa�ia  CodAlm  x(3)    Almac�n Almac�n     C�digo de almac�n   Descripcion X(40)   Descripci�n Descripci�n     Descripci�n de almac�n  �  ���������   �       �$                �     i     	       !   (     ��                                               �          ����                            undefined                                                               �           x   `                             �����               ��k        O   ����    e�          O   ����    R�          O   ����    ��      d        �   �           4   ����     /     �                                3   ����       $      8  ���                       8      
                       � ߱        x  �      D       `
     D          assignFocusedWidget         �      �     4       LOGICAL,INPUT pcName CHARACTER  assignWidgetValue   �      �          H       LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER  assignWidgetValueList   �      L      �    Z       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER  blankWidget d      �          p       LOGICAL,INPUT pcNameList CHARACTER  clearWidget �      0      \    |       LOGICAL,INPUT pcNameList CHARACTER  disableRadioButton  <      �      �    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    disableWidget   �      �      $    �       LOGICAL,INPUT pcNameList CHARACTER  enableRadioButton         H      |    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    enableWidget    \      �      �    �       LOGICAL,INPUT pcNameList CHARACTER  formattedWidgetValue    �            H  	  �       CHARACTER,INPUT pcName CHARACTER    formattedWidgetValueList    (      l      �  
  �       CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    hideWidget  �      �         
 �       LOGICAL,INPUT pcNameList CHARACTER  highlightWidget �      <      l          LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER  resetWidgetValue    L      �      �          LOGICAL,INPUT pcNameList CHARACTER  toggleWidget    �            8    "      LOGICAL,INPUT pcNameList CHARACTER  viewWidget        \      �   
 /      LOGICAL,INPUT pcNameList CHARACTER  widgetHandle    h      �      �    :      HANDLE,INPUT pcName CHARACTER   widgetLongcharValue �      �      0    G      LONGCHAR,INPUT pcName CHARACTER widgetIsBlank         P      �    [      LOGICAL,INPUT pcNameList CHARACTER  widgetIsFocused `      �      �    i      LOGICAL,INPUT pcName CHARACTER  widgetIsModified    �      �      (	    y      LOGICAL,INPUT pcNameList CHARACTER  widgetIsTrue    	      L	      |	    �      LOGICAL,INPUT pcName CHARACTER  widgetValue \	      �	      �	    �      CHARACTER,INPUT pcName CHARACTER    widgetValueList �	      �	      
    �      CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    `�     ;  x
  �
          4   ����d                 �
                      ��                  ;  ?                  �[X           ;  �
  <  	  <  ,                                        3   ����|       O   >  ��  ��  �   addRecord                               �  �      ��                  �  �  �              �Z�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            cancelRecord                                �  �      ��                  �  �  �              ]�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            copyRecord                              �  �      ��                  �  �  �              X�f        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableFields                               �  �      ��                  �  �  �              ��f        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            displayFields                               �  �      ��                  �  �  �              0�f        O   ����    e�          O   ����    R�          O   ����    ��            ��                             ��                            ����                            enableFields                                �  �      ��                  �  �                ���        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �  �      ��                  �                   ���        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            toolbar                             �  �      ��                                     p��        O   ����    e�          O   ����    R�          O   ����    ��            ��                             ��                            ����                            updateState                               �      ��                                    ��        O   ����    e�          O   ����    R�          O   ����    ��            ��                  4           ��                            ����                            valueChanged                                $        ��                  
    <              @��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewRecord                                       ��                      0              h�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            getTargetProcedure  �	      �      �    3      HANDLE, getObjectType   �      �      �    F      CHARACTER,  getShowPopup    �             0    T      LOGICAL,    setShowPopup          <      l    a      LOGICAL,INPUT plShowPopup LOGICAL   addRecord                                 �      ��                  �  �  ,              t��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            cancelRecord                                  �      ��                  �  �  $              ���        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            collectChanges                                �      ��                  �  �                ��        O   ����    e�          O   ����    R�          O   ����    ��            ��   h             4               ��                  \           ��                            ����                            confirmContinue                             L  4      ��                  �  �  d              �        O   ����    e�          O   ����    R�          O   ����    ��            ��                  |           ��                            ����                            confirmDelete                               l  T      ��                  �  �  �              ��        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            confirmExit                             �  p      ��                  �  �  �              �b        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            copyRecord                              �  �      ��                  �  �  �              c        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            dataAvailable                               �  �      ��                  �  �  �              <j        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            deleteRecord                                �   �       ��                  �  �  �               �bk        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �!  �!      ��                  �  �  �!              8ck        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            okToContinueProcedure                               �"  �"      ��                  �  �  �"              �fk        O   ����    e�          O   ����    R�          O   ����    ��            ��   #             �"               ��                  #           ��                            ����                            queryPosition                                $  �#      ��                  �  �  $              ���        O   ����    e�          O   ����    R�          O   ����    ��            ��                  0$           ��                            ����                            resetRecord                             %  %      ��                  �  �  4%               gk        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            showDataMessagesProcedure                                &  &      ��                  �  �  8&              (��        O   ����    e�          O   ����    R�          O   ����    ��            ��                  P&           ��                            ����                            updateMode                              <'  $'      ��                  �  �  T'              <Rj        O   ����    e�          O   ����    R�          O   ����    ��            ��                  l'           ��                            ����                            updateRecord                                \(  D(      ��                  �  �  t(              �Vj        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateState                             P)  8)      ��                  �  �  h)              8Wj        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �)           ��                            ����                            updateTitle                             l*  T*      ��                  �  �  �*              t�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            validateFields                              d+  L+      ��                  �  �  |+              �        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �+           ��                            ����                            getCreateHandles    L      �+      0,    n      CHARACTER,  getDataModified ,      <,      l,          LOGICAL,    getDisplayedFields  L,      x,      �,    �      CHARACTER,  getDisplayedTables  �,      �,      �,    �      CHARACTER,  getEnabledFields    �,      �,      ,-     �      CHARACTER,  getEnabledHandles   -      8-      l-  !  �      CHARACTER,  getFieldHandles L-      x-      �-  "  �      CHARACTER,  getFieldsEnabled    �-      �-      �-  #  �      LOGICAL,    getGroupAssignSource    �-      �-      ,.  $  �      HANDLE, getGroupAssignSourceEvents  .      4.      p.  %        CHARACTER,  getGroupAssignTarget    P.      |.      �.  &  )      CHARACTER,  getGroupAssignTargetEvents  �.      �.      �.  '  >      CHARACTER,  getNewRecord    �.      /      8/  (  Y      CHARACTER,  getObjectParent /      D/      t/  )  f      HANDLE, getRecordState  T/      |/      �/  *  v      CHARACTER,  getRowIdent �/      �/      �/  +  �      CHARACTER,  getTableIOSource    �/      �/      $0  ,  �      HANDLE, getTableIOSourceEvents  0      ,0      d0  -  �      CHARACTER,  getUpdateTarget D0      p0      �0  .  �      CHARACTER,  getUpdateTargetNames    �0      �0      �0  /  �      CHARACTER,  getWindowTitleField �0      �0      $1  0  �      CHARACTER,  okToContinue    1      01      `1  1  �      LOGICAL,INPUT pcAction CHARACTER    setContainerMode    @1      �1      �1  2  �      LOGICAL,INPUT pcContainerMode CHARACTER setDataModified �1      �1      2  3        LOGICAL,INPUT plModified LOGICAL    setDisplayedFields  �1      42      h2  4         LOGICAL,INPUT pcFieldList CHARACTER setEnabledFields    H2      �2      �2  5  3      LOGICAL,INPUT pcFieldList CHARACTER setGroupAssignSource    �2      �2      3  6  D      LOGICAL,INPUT phObject HANDLE   setGroupAssignSourceEvents  �2      <3      x3  7  Y      LOGICAL,INPUT pcEvents CHARACTER    setGroupAssignTarget    X3      �3      �3  8  t      LOGICAL,INPUT pcObject CHARACTER    setGroupAssignTargetEvents  �3      �3      44  9  �      LOGICAL,INPUT pcEvents CHARACTER    setLogicalObjectName    4      X4      �4  :  �      LOGICAL,INPUT pcLogicalObjectName CHARACTER setObjectParent p4      �4      �4  ;  �      LOGICAL,INPUT phParent HANDLE   setSaveSource   �4      5      <5  <  �      LOGICAL,INPUT plSave LOGICAL    setTableIOSource    5      \5      �5  =  �      LOGICAL,INPUT phObject HANDLE   setTableIOSourceEvents  p5      �5      �5  >  �      LOGICAL,INPUT pcEvents CHARACTER    setUpdateTarget �5      6      <6  ?  �      LOGICAL,INPUT pcObject CHARACTER    setUpdateTargetNames    6      `6      �6  @        LOGICAL,INPUT pcTargetNames CHARACTER   setWindowTitleField x6      �6      �6  A  $      LOGICAL,INPUT cWindowTitleField CHARACTER   showDataMessages    �6       7      T7  B  8      CHARACTER,  assignPageProperty                              �7  �7      ��                  �  �  8              H(f        O   ����    e�          O   ����    R�          O   ����    ��            ��   P8             8               ��                  D8           ��                            ����                            changePage                              09  9      ��                  �  �  H9              ��X        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmExit                             $:  :      ��                  �  �  <:              <�X        O   ����    e�          O   ����    R�          O   ����    ��            ��                  T:           ��                            ����                            constructObject                             D;  ,;      ��                  �  �  \;              ��        O   ����    e�          O   ����    R�          O   ����    ��            ��   �;             t;               �� 
  �;             �;  
             ��   �;             �;               �� 
                 �;  
         ��                            ����                            createObjects                               �<  �<      ��                  �  �  �<              ,@f        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deletePage                              �=  �=      ��                  �  �  �=              �Bf        O   ����    e�          O   ����    R�          O   ����    ��            ��                   >           ��                            ����                            destroyObject                               �>  �>      ��                  �  �  ?              Cf        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hidePage                                �?  �?      ��                  �     �?              (Jf        O   ����    e�          O   ����    R�          O   ����    ��            ��                  @           ��                            ����                            initializeObject                                A  �@      ��                       A               �        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeVisualContainer                               B  �A      ��                      $B              �        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initPages                                C  �B      ��                    
  C              (B�        O   ����    e�          O   ����    R�          O   ����    ��            ��                  0C           ��                            ����                            notifyPage                              D  D      ��                      4D              �F�        O   ����    e�          O   ����    R�          O   ����    ��            ��                  LD           ��                            ����                            passThrough                             8E   E      ��                      PE              �F�        O   ����    e�          O   ����    R�          O   ����    ��            ��   �E             hE               ��                  �E           ��                            ����                            removePageNTarget                               �F  lF      ��                      �F              0W�        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �F             �F  
             ��                  �F           ��                            ����                            selectPage                              �G  �G      ��                      �G              Lam        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �G           ��                            ����                            toolbar                             �H  �H      ��                       �H              `em        O   ����    e�          O   ����    R�          O   ����    ��            ��                  I           ��                            ����                            viewObject                              �I  �I      ��                  "  #  J              Tjm        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewPage                                �J  �J      ��                  %  '  K              �jm        O   ����    e�          O   ����    R�          O   ����    ��            ��                   K           ��                            ����                            disablePagesInFolder    47      �K      �K  C  I      LOGICAL,INPUT pcPageInformation CHARACTER   enablePagesInFolder �K      �K       L  D  ^      LOGICAL,INPUT pcPageInformation CHARACTER   getCallerProcedure   L      LL      �L  E  r      HANDLE, getCallerWindow `L      �L      �L  F  �      HANDLE, getContainerMode    �L      �L      �L  G  �      CHARACTER,  getContainerTarget  �L       M      4M  H  �      CHARACTER,  getContainerTargetEvents    M      @M      |M  I  �      CHARACTER,  getCurrentPage  \M      �M      �M  J  �      INTEGER,    getDisabledAddModeTabs  �M      �M      �M  K  �      CHARACTER,  getDynamicSDOProcedure  �M      N      @N  L  �      CHARACTER,  getFilterSource  N      LN      |N  M        HANDLE, getMultiInstanceActivated   \N      �N      �N  N        LOGICAL,    getMultiInstanceSupported   �N      �N      O  O  9      LOGICAL,    getNavigationSource �N      O      HO  P  S      CHARACTER,  getNavigationSourceEvents   (O      TO      �O  Q  g      CHARACTER,  getNavigationTarget pO      �O      �O  R  �      HANDLE, getOutMessageTarget �O      �O      P  S  �      HANDLE, getPageNTarget  �O      P      DP  T  �      CHARACTER,  getPageSource   $P      PP      �P  U  �      HANDLE, getPrimarySdoTarget `P      �P      �P  V  �      HANDLE, getReEnableDataLinks    �P      �P      �P  W  �      CHARACTER,  getRunDOOptions �P      Q      8Q  X  �      CHARACTER,  getRunMultiple  Q      DQ      tQ  Y  �      LOGICAL,    getSavedContainerMode   TQ      �Q      �Q  Z        CHARACTER,  getSdoForeignFields �Q      �Q      �Q  [  $      CHARACTER,  getTopOnly  �Q      R      0R  \ 
 8      LOGICAL,    getUpdateSource R      <R      lR  ]  C      CHARACTER,  getWaitForObject    LR      xR      �R  ^  S      HANDLE, getWindowTitleViewer    �R      �R      �R  _  d      HANDLE, getStatusArea   �R      �R      $S  `  y      LOGICAL,    pageNTargets    S      0S      `S  a  �      CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER setCallerObject @S      �S      �S  b  �      LOGICAL,INPUT h HANDLE  setCallerProcedure  �S      �S      T  c  �      LOGICAL,INPUT h HANDLE  setCallerWindow �S      ,T      \T  d  �      LOGICAL,INPUT h HANDLE  setContainerTarget  <T      tT      �T  e  �      LOGICAL,INPUT pcObject CHARACTER    setCurrentPage  �T      �T      �T  f  �      LOGICAL,INPUT iPage INTEGER setDisabledAddModeTabs  �T      U      PU  g  �      LOGICAL,INPUT cDisabledAddModeTabs CHARACTER    setDynamicSDOProcedure  0U      �U      �U  h         LOGICAL,INPUT pcProc CHARACTER  setFilterSource �U      �U      V  i        LOGICAL,INPUT phObject HANDLE   setInMessageTarget  �U      (V      \V  j  '      LOGICAL,INPUT phObject HANDLE   setMultiInstanceActivated   <V      |V      �V  k  :      LOGICAL,INPUT lMultiInstanceActivated LOGICAL   setMultiInstanceSupported   �V      �V      $W  l  T      LOGICAL,INPUT lMultiInstanceSupported LOGICAL   setNavigationSource W      TW      �W  m  n      LOGICAL,INPUT pcSource CHARACTER    setNavigationSourceEvents   hW      �W      �W  n  �      LOGICAL,INPUT pcEvents CHARACTER    setNavigationTarget �W      X      @X  o  �      LOGICAL,INPUT cTarget CHARACTER setOutMessageTarget  X      `X      �X  p  �      LOGICAL,INPUT phObject HANDLE   setPageNTarget  tX      �X      �X  q  �      LOGICAL,INPUT pcObject CHARACTER    setPageSource   �X      Y      8Y  r  �      LOGICAL,INPUT phObject HANDLE   setPrimarySdoTarget Y      XY      �Y  s  �      LOGICAL,INPUT hPrimarySdoTarget HANDLE  setReEnableDataLinks    lY      �Y      �Y  t  �      LOGICAL,INPUT cReEnableDataLinks CHARACTER  setRouterTarget �Y      Z      HZ  u  
	      LOGICAL,INPUT phObject HANDLE   setRunDOOptions (Z      hZ      �Z  v  	      LOGICAL,INPUT pcOptions CHARACTER   setRunMultiple  xZ      �Z      �Z  w  *	      LOGICAL,INPUT plMultiple LOGICAL    setSavedContainerMode   �Z      [      H[  x  9	      LOGICAL,INPUT cSavedContainerMode CHARACTER setSdoForeignFields ([      t[      �[  y  O	      LOGICAL,INPUT cSdoForeignFields CHARACTER   setTopOnly  �[      �[       \  z 
 c	      LOGICAL,INPUT plTopOnly LOGICAL setUpdateSource �[       \      P\  {  n	      LOGICAL,INPUT pcSource CHARACTER    setWaitForObject    0\      t\      �\  |  ~	      LOGICAL,INPUT phObject HANDLE   setWindowTitleViewer    �\      �\       ]  }  �	      LOGICAL,INPUT phViewer HANDLE   setStatusArea   �\       ]      P]  ~  �	      LOGICAL,INPUT plStatusArea LOGICAL  applyLayout                             �]  �]      ��                  �  �  ^              �f        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableObject                               �^  �^      ��                  �  �  _              ��f        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            enableObject                                �_  �_      ��                  �  �   `              X�f        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �`  �`      ��                  �  �  �`              ��f        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            processAction                               �a  �a      ��                  �  �  �a              ��f        O   ����    e�          O   ����    R�          O   ����    ��            ��                  b           ��                            ����                            getAllFieldHandles  0]      tb      �b    �	      CHARACTER,  getAllFieldNames    �b      �b      �b  �  �	      CHARACTER,  getCol  �b      �b      c  �  �	      DECIMAL,    getDefaultLayout    �b      (c      \c  �  �	      CHARACTER,  getDisableOnInit    <c      hc      �c  �  �	      LOGICAL,    getEnabledObjFlds   |c      �c      �c  �  �	      CHARACTER,  getEnabledObjHdls   �c      �c      d  �  
      CHARACTER,  getHeight   �c      (d      Td  � 	 #
      DECIMAL,    getHideOnInit   4d      `d      �d  �  -
      LOGICAL,    getLayoutOptions    pd      �d      �d  �  ;
      CHARACTER,  getLayoutVariable   �d      �d      e  �  L
      CHARACTER,  getObjectEnabled    �d      e      Pe  �  ^
      LOGICAL,    getObjectLayout 0e      \e      �e  �  o
      CHARACTER,  getRow  le      �e      �e  �  
      DECIMAL,    getWidth    �e      �e      �e  �  �
      DECIMAL,    getResizeHorizontal �e      f      8f  �  �
      LOGICAL,    getResizeVertical   f      Df      xf  �  �
      LOGICAL,    setAllFieldHandles  Xf      �f      �f  �  �
      LOGICAL,INPUT pcValue CHARACTER setAllFieldNames    �f      �f      g  �  �
      LOGICAL,INPUT pcValue CHARACTER setDefaultLayout    �f      ,g      `g  �  �
      LOGICAL,INPUT pcDefault CHARACTER   setDisableOnInit    @g      �g      �g  �  �
      LOGICAL,INPUT plDisable LOGICAL setHideOnInit   �g      �g      h  �  �
      LOGICAL,INPUT plHide LOGICAL    setLayoutOptions    �g      (h      \h  �  	      LOGICAL,INPUT pcOptions CHARACTER   setObjectLayout <h      �h      �h  �        LOGICAL,INPUT pcLayout CHARACTER    setResizeHorizontal �h      �h      i  �  *      LOGICAL,INPUT plResizeHorizontal LOGICAL    setResizeVertical   �h      4i      hi  �  >      LOGICAL,INPUT plResizeVertical LOGICAL  getObjectTranslated Hi      �i      �i  �  P      LOGICAL,    getObjectSecured    �i      �i      j  �  d      LOGICAL,    createUiEvents  �i      j      @j  �  u      LOGICAL,    bindServer                              �j  �j      ��                  �  �  �j              �0        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �k  �k      ��                  �  �  �k              �7        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �l  �l      ��                  �  �  �l              �:        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �m  �m      ��                  �  �  �m              �=        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �n  �n      ��                  �  �  �n              $>        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �o  �o      ��                  �  �  �o              �>        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �p  �p      ��                  �  �  �p              0F�        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �p  
         ��                            ����                            startServerObject                               �q  �q      ��                  �  �  �q              |J�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                �r  �r      ��                  �  �  �r              0K�        O   ����    e�          O   ����    R�          O   ����    ��            ��                   s           ��                            ����                            getAppService    j      hs      �s  �  �      CHARACTER,  getASBound  xs      �s      �s  � 
 �      LOGICAL,    getAsDivision   �s      �s      t  �  �      CHARACTER,  getASHandle �s      t      Dt  �  �      HANDLE, getASHasStarted $t      Lt      |t  �  �      LOGICAL,    getASInfo   \t      �t      �t  � 	 �      CHARACTER,  getASInitializeOnRun    �t      �t      �t  �  �      LOGICAL,    getASUsePrompt  �t      u      4u  �  �      LOGICAL,    getServerFileName   u      @u      tu  �  �      CHARACTER,  getServerOperatingMode  Tu      �u      �u  �        CHARACTER,  runServerProcedure  �u      �u      �u  �        HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   �u      <v      lv  �  1      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   Lv      �v      �v  �  ?      LOGICAL,INPUT pcDivision CHARACTER  setASHandle �v      �v      w  �  M      LOGICAL,INPUT phASHandle HANDLE setASInfo   �v      4w      `w  � 	 Y      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    @w      �w      �w  �  c      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �w      �w      x  �  x      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   �w      ,x      `x  �  �      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  @x      �x      �x  �  �      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             ly  Ty      ��                  q  u  �y              ��g        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �y             �y  
             ��   �y             �y               �� 
                 �y  
         ��                            ����                            addMessage                              �z  �z      ��                  w  {  �z              ��g        O   ����    e�          O   ����    R�          O   ����    ��            ��   <{             {               ��   d{             0{               ��                  X{           ��                            ����                            adjustTabOrder                              H|  0|      ��                  }  �  `|              �~�        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �|             x|  
             �� 
  �|             �|  
             ��                  �|           ��                            ����                            applyEntry                              �}  �}      ��                  �  �  �}              |��        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �}           ��                            ����                            changeCursor                                �~  �~      ��                  �  �  �~              ���        O   ����    e�          O   ����    R�          O   ����    ��            ��                             ��                            ����                            createControls                              �  �      ��                  �  �  �              ��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �  Ԁ      ��                  �  �  �              ���        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �  ́      ��                  �  �  ��              x��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �  ̂      ��                  �  �  ��              ���        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              ؃  ��      ��                  �  �  ��              ���        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              ̄  ��      ��                  �  �  �              ���        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                ȅ  ��      ��                  �  �  ��              0��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              Ć  ��      ��                  �  �  ܆              ���        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  (�             �  
             ��   P�             �               ��   x�             D�               ��                  l�           ��                            ����                            modifyUserLinks                             \�  D�      ��                  �  �  t�              ��        O   ����    e�          O   ����    R�          O   ����    ��            ��   ��             ��               ��   �             ��               �� 
                 ܈  
         ��                            ����                            removeAllLinks                              ̉  ��      ��                  �  �  �              (�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              ��  ��      ��                  �  �  ؊               �        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  $�             ��  
             ��   L�             �               �� 
                 @�  
         ��                            ����                            repositionObject                                4�  �      ��                  �  �  L�              ��        O   ����    e�          O   ����    R�          O   ����    ��            ��   ��             d�               ��                  ��           ��                            ����                            returnFocus                             x�  `�      ��                  �  �  ��              @�        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 ��  
         ��                            ����                            showMessageProcedure                                ��  ��      ��                  �  �  ��              $�        O   ����    e�          O   ����    R�          O   ����    ��            ��   �             Ў               ��                  ��           ��                            ����                            toggleData                              �  ̏      ��                  �  �  ��              ��        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            viewObject                               �  �      ��                  �  �  �              �        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �x      p�      ��  � 
 �      LOGICAL,    assignLinkProperty  |�      ��      ܑ  �  	      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   ��      4�      d�  �        CHARACTER,  getChildDataKey D�      p�      ��  �  *      CHARACTER,  getContainerHandle  ��      ��      ��  �  :      HANDLE, getContainerHidden  ��      �      �  �  M      LOGICAL,    getContainerSource  ��      (�      \�  �  `      HANDLE, getContainerSourceEvents    <�      d�      ��  �  s      CHARACTER,  getContainerType    ��      ��      ��  �  �      CHARACTER,  getDataLinksEnabled ��      �       �  �  �      LOGICAL,    getDataSource    �      ,�      \�  �  �      HANDLE, getDataSourceEvents <�      d�      ��  �  �      CHARACTER,  getDataSourceNames  x�      ��      ؔ  �  �      CHARACTER,  getDataTarget   ��      �      �  �  �      CHARACTER,  getDataTargetEvents ��       �      T�  �  �      CHARACTER,  getDBAware  4�      `�      ��  � 
       LOGICAL,    getDesignDataObject l�      ��      ̕  �        CHARACTER,  getDynamicObject    ��      ؕ      �  �  '      LOGICAL,    getInstanceProperties   �      �      P�  �  8      CHARACTER,  getLogicalObjectName    0�      \�      ��  �  N      CHARACTER,  getLogicalVersion   t�      ��      Ԗ  �  c      CHARACTER,  getObjectHidden ��      ��      �  �  u      LOGICAL,    getObjectInitialized    �      �      T�  �  �      LOGICAL,    getObjectName   4�      `�      ��  �  �      CHARACTER,  getObjectPage   p�      ��      ̗  �  �      INTEGER,    getObjectVersion    ��      ؗ      �  �  �      CHARACTER,  getObjectVersionNumber  �      �      P�  �  �      CHARACTER,  getParentDataKey    0�      \�      ��  �  �      CHARACTER,  getPassThroughLinks p�      ��      И  �  �      CHARACTER,  getPhysicalObjectName   ��      ܘ      �  �        CHARACTER,  getPhysicalVersion  ��       �      T�  �        CHARACTER,  getPropertyDialog   4�      `�      ��  �  ,      CHARACTER,  getQueryObject  t�      ��      Й  �  >      LOGICAL,    getRunAttribute ��      ܙ      �  �  M      CHARACTER,  getSupportedLinks   �      �      L�  �  ]      CHARACTER,  getTranslatableProperties   ,�      X�      ��  �  o      CHARACTER,  getUIBMode  t�      ��      ̚  � 
 �      CHARACTER,  getUserProperty ��      ؚ      �  �  �      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    �      0�      h�  �  �      CHARACTER,INPUT pcPropList CHARACTER    linkHandles H�      ��      ��  �  �      CHARACTER,INPUT pcLink CHARACTER    linkProperty    ��      ��      �  �  �      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry �      L�      x�  �  �      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   X�      �      �  �  �      CHARACTER,INPUT piMessage INTEGER   propertyType    ��      8�      h�  �  �      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  H�      ��      ��  �  �      CHARACTER,  setChildDataKey ��      ̝      ��  �        LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  ܝ      $�      X�  �        LOGICAL,INPUT plHidden LOGICAL  setContainerSource  8�      x�      ��  �  +      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    ��      ̞      �  �  >      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled �      ,�      `�  �  W      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   @�      ��      ��  �  k      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents ��      ؟      �  �  y      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  �      4�      h�  �  �      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   H�      ��      ��  �  �      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents ��      �      �  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  ��      <�      h�  � 
 �      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject H�      ��      ��  �  �      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    ��      �      �  �  �      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   ��      4�      l�  �  �      LOGICAL,INPUT pcPropList CHARACTER  setLogicalVersion   L�      ��      Ģ  �        LOGICAL,INPUT cVersion CHARACTER    setObjectName   ��      �      �  �        LOGICAL,INPUT pcName CHARACTER  setObjectVersion    ��      8�      l�  �  (      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    L�      ��      ȣ  �  9      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks ��      �      $�  �  J      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   �      D�      |�  �  ^      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  \�      ��      Ф  �  t      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute ��      ��      $�  �  �      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   �      L�      ��  �  �      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   `�      ��      �  �  �      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  ��      �      0�  � 
 �      LOGICAL,INPUT pcMode CHARACTER  setUserProperty �      P�      ��  �  �      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage `�      ��      �  �  �      LOGICAL,INPUT pcMessage CHARACTER   Signature   ̦      �      <�  � 	 �      CHARACTER,INPUT pcName CHARACTER    �     �	  x�  �          4   �����                 ��                      ��                  �	  
                  Dn           �	  ��         �	  �  ��          4   �����                 ��                      ��                  �	  
                  �n           �	   �  ��     �	  ��  �          4   �����                 (�                      ��                  
  
                  Ln           
  ��         
                                  d     
                    � ߱        ��  $   	
  T�  ���                           $   
  ة  ���                       �                         � ߱        ��     
  �  ��          4   �����                ��                      ��                  
  �
                   	n           
  ,�  Ъ  o   
      ,                                 (�  $   
  ��  ���                       4  @                        � ߱        <�  �   
  T      P�  �   
  �      d�  �   
  <      x�  �   
  �      ��  �   
  $      ��  �    
  �      ��  �   !
        ȫ  �   "
  P      ܫ  �   %
  �      �  �   '
  8      �  �   (
  �      �  �   *
  0      ,�  �   +
  �      @�  �   ,
  �      T�  �   -
  d      h�  �   .
  �      |�  �   4
  	      ��  �   6
  �	      ��  �   <
  �	      ��  �   >
  8
      ̬  �   @
  �
      �  �   A
  (      ��  �   G
  �      �  �   H
        �  �   I
  �      0�  �   J
        D�  �   M
  |      X�  �   N
  �      l�  �   P
  ,      ��  �   Q
  h      ��  �   S
  �      ��  �   T
        ��  �   U
  T      Э  �   V
  �      �  �   W
  �      ��  �   X
  H      �  �   Y
  �       �  �   [
  �      4�  �   \
  �      H�  �   ]
  8      \�  �   _
  t      p�  �   `
  �      ��  �   a
  �      ��  �   b
  (          �   c
  d                      ��          $�  �      ��                  �
  +  <�              �lf        O   ����    e�          O   ����    R�          O   ����    ��      �     
                P                     `                         � ߱        �  $    T�  ���                           O   )  ��  ��  �               P�          @�  H�    0�                                             ��                            ����                                �      ��      ��     T     X�                       T�  F                     ��     K  �  |�          4   �����                ��                      ��                  L  �                  �if           L  �  ��  �   O        ��  �   P  �      ȱ  �   Q  �      ܱ  �   R  x      �  �   S  �      �  �   T  p      �  �   U  �      ,�  �   V  `      @�  �   W  �      T�  �   X  X      h�  �   Y  �      |�  �   Z  H      ��  �   [  �          �   \  @      l�     �  ��  ,�          4   �����                <�                      ��                  �  l                  �+           �  ̲  P�  �   �        d�  �   �  �      x�  �   �  �      ��  �   �  t      ��  �   �  �      ��  �   �  \      ȳ  �   �  �      ܳ  �   �  L      �  �   �  �      �  �   �  4       �  �   �  �       ,�  �   �  $!      @�  �   �  �!      T�  �   �  "      h�  �   �  �"      |�  �   �  #      ��  �   �  �#      ��  �   �  $      ��  �   �  �$      ̴  �   �  �$      �  �   �  x%      ��  �   �  �%      �  �   �  p&      �  �   �  �&      0�  �   �  h'      D�  �   �  �'      X�  �   �  `(          �   �  �(      x�     x  ��  ��          4   ����D)                �                      ��                  y  *                  �-           y  ��  �  �   |  �)      ,�  �   }   *      @�  �   ~  �*      T�  �     +      h�  �   �  �+      |�  �   �  �+      ��  �   �  l,      ��  �   �  �,      ��  �   �  -      ̶  �   �  X-      �  �   �  �-      ��  �   �  .      �  �   �  |.      �  �   �  �.      0�  �   �  l/      D�  �   �  �/      X�  �   �  T0      l�  �   �  �0      ��  �   �  L1      ��  �   �  �1      ��  �   �  �1      ��  �   �  p2      з  �   �  �2      �  �   �   3      ��  �   �  \3      �  �   �  �3       �  �   �  4      4�  �   �  P4      H�  �   �  �4      \�  �   �  �4      p�  �   �  5      ��  �   �  @5      ��  �   �  |5      ��  �   �  �5      ��  �   �  ,6      Ը  �   �  h6      �  �   �  �6      ��  �   �  �6      �  �   �  7      $�  �   �  X7      8�  �   �  �7      L�  �   �  8      `�  �   �  |8      t�  �   �  �8      ��  �   �  d9      ��  �   �  �9      ��  �   �  \:      Ĺ  �   �  �:      ع  �   �  T;      �  �   �  �;       �  �   �  L<      �  �   �  �<      (�  �   �  =      <�  �   �  @=      P�  �   �  |=      d�  �   �  �=          �   �  ,>      |�     8  ��   �          4   �����>  	              �                      ��             	     9  �                  �q�           9  ��  $�  �   ;  �>      8�  �   <  h?      L�  �   =  �?      `�  �   >  X@      t�  �   D  �@      ��  �   E  hA      ��  �   F  �A      ��  �   G  PB      Ļ  �   H  �B      ػ  �   I  HC      �  �   J  �C       �  �   K  8D      �  �   L  tD      (�  �   N  �D      <�  �   O  \E      P�  �   P  �E      d�  �   Q  DF      x�  �   R  �F      ��  �   S  ,G      ��  �   T  �G      ��  �   U  H      ȼ  �   V  �H      ܼ  �   W  I      �  �   X  �I      �  �   Y  �I      �  �   [  0J      ,�  �   \  �J      @�  �   ^  K      T�  �   _  �K      h�  �   `  L          �   a  �L      P�     �  ��  �          4   �����L  
              �                      ��             
     �  V                  V�           �  ��  (�  �   �  M      <�  �   �  �M          �   �  N      �       h�  ؾ          4   ����<N                �                      ��                    "                  4X�             x�  h�        �  �          4   ����TN      $    <�  ���                       �N  @         �N              � ߱                 ��  ��          4   �����N      $     ��  ���                       O  @         �N              � ߱        @�  $   *  �  ���                       <O     
                    � ߱        ��     c  X�  h�          4   ����PO      /   d  ��     ��                          3   ����`O            ��                      3   �����O  �     m  ��  \�  <�      4   �����O                l�                      ��                  n  �                  �7�           n  ��  ��  �   r  �O      ��  $   s  ��  ���                       (P     
                    � ߱        ��  �   t  HP      D�  $   v  �  ���                       pP  @         \P              � ߱         �  $   y  p�  ���                       �P                         � ߱        �Q     
                R                     \S  @        
 S              � ߱        ��  V   �  ��  ���                        hS                     �S       	       	       �S                         � ߱         �  $   �  ,�  ���                       �T     
                U                     dV  @        
 $V              � ߱        ��  V   �  ��  ���                        pV     
                �V                     <X  @        
 �W              � ߱            V   �  L�  ���                                      �                      ��                  �  �                  09�           �  ��  PX     
                �X                     Z  @        
 �Y          �Z  @        
 DZ          �Z  @        
 �Z          D[  @        
 [              � ߱            V   	  L�  ���                        adm-clone-props ��  0�              �     U     4                          0  �"                     start-super-proc    @�  ��  �           �     V                                  �"                     ��     �  $�  4�          4   �����^      /   �  `�     p�                          3   �����^            ��                      3   ���� _  ��  $   �  ��  ���                        _       
       
           � ߱        ��     �  �  ��   �      4   ����<_                ��                      ��                  �  �                  ��f           �   �  P_       
       
       d_                     x_                         � ߱            $   �  ��  ���                              �  8�  t�          4   �����_  �_       
       
           � ߱            $   �  H�  ���                       ��     �  ��  ��   �      4   �����_      $   �  ��  ���                       �_                         � ߱            �      �_      8`     
                �`                     b  @        
 �a              � ߱        ��  V     4�  ���                        ��  �   G  b      l�     �  ��   �          4   ����Pb      /   �  ,�     <�                          3   ����`b            \�                      3   �����b  0�     1  ��  ��          4   �����b                �                      ��                  2  5                  ��f           2  ��      g   3  �         ����                           ��          ��  ��      ��                  4      ��              �f        O   ����    e�          O   ����    R�          O   ����    ��          /  4  �     �  �b                      3   �����b  D�     
   4�                      3   �����b         
   d�                      3   �����b    ��                              ��        �                  ����                                        0�              W      t�                      g                               ,�  g   7  H�          ��	��                           �          ��  ��      ��                  7  9  ��              h�         O   ����    e�          O   ����    R�          O   ����    ��          /  8  0�     @�  �b                      3   �����b            `�                      3   ����c    ��                              ��        �                  ����                                        \�              X      p�                      g                               (�  g   ;  D�          ��	��                            �          ��  ��      ��                  ;  =  ��              �         O   ����    e�          O   ����    R�          O   ����    ��          /  <  ,�     <�  <c                      3   ���� c            \�                      3   ����Dc    ��                              ��        �                  ����                                        X�              Y      l�                      g                               p�     T  @�  ��          4   ����`c                ��                      ��                  U  t                  Ppe           U  P�  ,�  /   V  ��     ��                          3   ����pc            �                      3   �����c  (�  /  X  X�     h�  �c                      3   �����c  ��     
   ��                      3   �����c  ��        ��                      3   �����c  ��        ��                      3   �����c            �                      3   ����d  L�     `  @�  P�          4   ����8d      /  f  |�     ��  �d                      3   �����d  ��     
   ��                      3   �����d  ��        ��                      3   �����d  �        �                      3   �����d            <�                      3   ����e         l  d�  t�          4   ����(e      /  o  ��     ��  |e                      3   ����\e  ��     
   ��                      3   �����e  �         �                      3   �����e  @�        0�                      3   �����e            `�                      3   �����e  �     �  �e                                     �e     
                pf                     �g  @        
 �g              � ߱        ��  V   �  ��  ���                        �g     
                Ph                     �i  @        
 `i              � ߱        ��  V     4�  ���                        0�     F  ��  L�          4   �����i                \�                      ��                  G  L                  0��           G  ��  ��  /   H  ��     ��                          3   �����i            ��                      3   �����i      /   J  ��     �                          3   ���� j  4�     
   $�                      3   ���� j  d�        T�                      3   ����(j  ��        ��                      3   ����<j            ��                      3   ����Xj  displayObjects  ��  ��                      Z      �                               8$                     ��  g   �  H�         �4��                           �          ��  ��      ��                  �      ��              D'f        O   ����    e�          O   ����    R�          O   ����    ��          /  �  0�         �j                      3   ����tj    ��                              ��        �                  ����                                        \�              [      @�                      g                               ��  g   �  �          �0L�      }                      ��          ��  ��      ��                  �      ��              (�        O   ����    e�          O   ����    R�          O   ����    ��          /  �  ��         �j                      3   �����j    ��                            ����                                        (�              \      �                      g                               (�     �  ��  0�          4   �����j                @�                      ��                  �                    ��           �  ��  ��  /   �  l�     |�                          3   �����j            ��                      3   �����j      /  �  ��     ��  (k                      3   ����k  �     
   �                      3   ����0k  H�        8�                      3   ����8k  x�        h�                      3   ����Lk            ��                      3   ����lk  �k                     �k                     �k                     8l                         � ߱        ��  $     ��  ���                       �l     
                m                     Xn  @        
 n          �n  @        
 pn          o  @        
 �n              � ߱        d�  V     T�  ���                        0o  @         o          Xo  @         Do              � ߱            $     �  ���                       disable_UI  ��  ��                      ]                                    �$  
                    �  �   ���  �                $�  0�      toggleData  ,INPUT plEnabled LOGICAL    �  \�  t�      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  L�  ��  ��      returnFocus ,INPUT hTarget HANDLE   ��  ��   �      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    ��  <�  H�      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ,�  ��  ��      removeAllLinks  ,   ��  ��  ��      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE ��  (�  <�      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    �  ��  ��      hideObject  ,   ��  ��  ��      exitObject  ,   ��  ��  �      editInstanceProperties  ,   ��   �  0�      displayLinks    ,   �  D�  T�      createControls  ,   4�  h�  x�      changeCursor    ,INPUT pcCursor CHARACTER   X�  ��  ��      applyEntry  ,INPUT pcField CHARACTER    ��  ��  ��      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER ��  D�  P�      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER 4�  ��  ��      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  �  �      unbindServer    ,INPUT pcMode CHARACTER ��  <�  P�      startServerObject   ,   ,�  d�  t�      runServerObject ,INPUT phAppService HANDLE  T�  ��  ��      restartServerObject ,   ��  ��  ��      initializeServerObject  ,   ��  ��  �      disconnectObject    ,   ��  �  0�      destroyServerObject ,   �  D�  P�      bindServer  ,   4�  d�  t�      processAction   ,INPUT pcAction CHARACTER   T�  ��  ��      enableObject    ,   ��  ��  ��      disableObject   ,   ��  ��  ��      applyLayout ,   ��  �  �      viewPage    ,INPUT piPageNum INTEGER    ��  @�  L�      viewObject  ,   0�  `�  l�      selectPage  ,INPUT piPageNum INTEGER    P�  ��  ��      removePageNTarget   ,INPUT phTarget HANDLE,INPUT piPage INTEGER ��  ��  ��      passThrough ,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER  ��  <�  H�      notifyPage  ,INPUT pcProc CHARACTER ,�  p�  |�      initPages   ,INPUT pcPageList CHARACTER `�  ��  ��      initializeVisualContainer   ,   ��  ��  ��      hidePage    ,INPUT piPageNum INTEGER    ��  �   �      destroyObject   ,    �  4�  @�      deletePage  ,INPUT piPageNum INTEGER    $�  l�  |�      createObjects   ,   \�  ��  ��      constructObject ,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE ��  �   �      changePage  ,   �  4�  H�      assignPageProperty  ,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER $�  ��  ��      validateFields  ,INPUT-OUTPUT pcNotValidFields CHARACTER    x�  ��  ��      updateTitle ,   ��  ��  �      updateRecord    ,   ��  �  $�      updateMode  ,INPUT pcMode CHARACTER �  L�  h�      showDataMessagesProcedure   ,OUTPUT pcReturn CHARACTER  <�  ��  ��      resetRecord ,   ��  ��  ��      queryPosition   ,INPUT pcState CHARACTER    ��  ��  �      okToContinueProcedure   ,INPUT pcAction CHARACTER,OUTPUT plAnswer LOGICAL   ��  L�  \�      deleteRecord    ,   <�  p�  ��      dataAvailable   ,INPUT pcRelative CHARACTER `�  ��  ��      confirmExit ,INPUT-OUTPUT plCancel LOGICAL  ��  ��  ��      confirmDelete   ,INPUT-OUTPUT plAnswer LOGICAL  ��  (�  8�      confirmContinue ,INPUT-OUTPUT plCancel LOGICAL  �  h�  x�      collectChanges  ,INPUT-OUTPUT pcChanges CHARACTER,INPUT-OUTPUT pcInfo CHARACTER X�  ��  ��      viewRecord  ,   ��  ��  ��      valueChanged    ,   ��  �  �      updateState ,INPUT pcState CHARACTER    ��  D�  L�      toolbar ,INPUT pcValue CHARACTER    4�  x�  ��      initializeObject    ,   h�  ��  ��      enableFields    ,   ��  ��  ��      displayFields   ,INPUT pcColValues CHARACTER    ��  �  �      disableFields   ,INPUT pcFieldType CHARACTER    ��  D�  P�      copyRecord  ,   4�  d�  t�      cancelRecord    ,   T�  ��  ��      addRecord   ,        � 
"     
 �%     adecomm/as-utils.w  
"   
   �    }        �
"     
    �     }        �� �  D   %               � 
"    
 � %              � �  �         `      $              
�    � �   �      
�             �G                      
�            � �   � 
"    
 n
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �            7%               
"   
 �           H    1�   
 �    � %               o%   o           �     
"   
 �           �    1�    �    � %               o%   o           � %   
"   
 �           0    1� ,  
 �    � %               o%   o           � 7   
"   
 �           �    1� G   �    � %               o%   o           � U   
"   
 �               1� [   �    � %               o%   o           � j   
"   
 �           �    1� �   � �   � %               o%   o           %               
"   
 � �              1� �   � � �     
"   
 �           D    1� �   �    � %               o%   o           � �  � 
"   
 �           �    1� ~   �    � %               o%   o           � �  N 
"   
 �           ,    1� �   � �   � %               o%   o           %               
"   
 �           �    1� �   � �   � %               o%   o           %               
"   
 �           $    1� �   � �   � %               o%   o           %              
"   
 � �          �    1�    � � �     
"   
 �           �    1�   
 � �   � %               o%   o           %               
"   
 �           X    1� %   �    � %               o%   o           �     
"   
 � �          �    1� -   � � �     
"   
 �           	    1� =   �    � %               o%   o           � S  t 
"   
 � �          |	    1� �  
 � � �     
"   
 �           �	    1� �   �    � %               o%   o           � �  � 
"   
 �           ,
    1� q   �    � %               o%   o           �     
"   
 �           �
    1� �  
 � �   � %               o%   o           %               
"   
 f�               1� �   f� �   � %               o%   o           %               
"   
 n�           �    1� �   n�    � %               o%   o           �     f
"   
 n�               1� �   n�    � %               o%   o           o%   o           
"   
 n�           �    1� �  
 n�    � %               o%   o           �     f
"   
 n�           �    1� �   n� �  	 � %               o%   o           � �  / n
"   
 � �          p    1�    � � �  	   
"   
 f�           �    1� (   f� �  	 � o%   o           o%   o           �     f
"   
 � �               1� ;   � � �  	   
"   
 n�           \    1� J   n� �  	 � o%   o           o%   o           �     n
"   
 � �          �    1� Z   � � �     
"   
 � �              1� h   � � �  	   
"   
 � �          H    1� u   � � �  	   
"   
 � �          �    1� �   � � �  	   
"   
 g�           �    1� �   g� �   � o%   o           o%   o           %              
"   
 � �          <    1� �   � � �  	   
"   
 � �          x    1� �  
 � � �     
"   
 � �          �    1� �   � � �  	   
"   
 � �          �    1� �   � � �  	   
"   
 � �          ,    1� �   � � �  	   
"   
 � �          h    1� �   � � �  	   
"   
 � �          �    1�   	 � � �  	   
"   
 � �          �    1�    � � �  	   
"   
 � �              1� %   � � �  	   
"   
 n�           X    1� <   n�    � %               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 f
"   
   
"   
 o(�  L ( l       �             �� H   � P   �        ,    �@    
� @  , 
�       8    �� Q     p�               �L
�    %              � 8      D    � $         � X          
�    � r     
"   
 �� @  , 
�       T    �� ,  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 f�                1� u  
 f�    � %               o%   o           �     f
"   
 f�           t    1� �  
 f�    � %               o%   o           o%   o           
"   
 f�           �    1� �   f� �   � %               o%   o           o%   o           
"   
 n�           l    1� �   n� �   � %               o%   o           %               
"   
 f�           �    1� �   f� �   � %               o%   o           %               
"   
 ��           d    1� �   ��    � %               o%   o           �     f
"   
 g�           �    1� �   g� �   � %               o%   o           %              
"   
 g�           T    1� �   g� �   � %               o%   o           o%   o           
"   
 n�           �    1� �   n�    � %               o%   o           o%   o           
"   
 f�           L    1� �  	 f�    � %               o%   o           �     f
"   
 f�           �    1� �   f�    � %               o%   o           o%   o           
"   
 f�           <    1�    f�    � %               o%   o           o%   o           
"   
 f�           �    1�    f� �   � %               o%   o           %               
"   
 f�           4    1�     f� �   � %               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 f�               1� ,   f� �  	 � %               o%   o           �     f
"   
 n�           x    1� 9   n� �  	 � %               o%   o           �     f
"   
 f�           �    1� G   f� �   � %               o%   o           %               
"   
 ��           h    1� U   �� �  	 � %               o%   o           �     f
"   
 f�           �    1� d   f� �  	 � %               o%   o           �     �
"   
 g�           P    1� r   g� �   � %               o%   o           %               
"   
 n�           �    1� �   n� �  	 � %               o%   o           �     g
"   
 f�           @    1� �   f� �  	 � %               o%   o           �     n
"   
 f�           �    1� �   f� �  	 � %               o%   o           �     f
"   
 f�           (     1� �   f� �  	 � %               o%   o           o%   o           
"   
 f�           �     1� �   f� �  	 � %               o%   o           �     n
"   
 ��           !    1� �   �� �  	 � %               o%   o           �     f
"   
 f�           �!    1� �  	 f� �   � %               o%   o           %               
"   
 g�           "    1� �   g� �   � %               o%   o           %               
"   
 g�           �"    1� �   g� �   � %               o%   o           o%   o           
"   
 n�            #    1� �   n� �   � %               o%   o           o%   o           
"   
 f�           |#    1�    f� �   � %               o%   o           %               
"   
 n�           �#    1�    n� �   � %               o%   o           %               
"   
 f�           t$    1� *   f� �   � %               o%   o           %               
"   
 ��           �$    1� ?   �� K   � %               o%   o           %       
       
"   
 ��           l%    1� S   �� K   � %               o%   o           o%   o           
"   
 f�           �%    1� _   f� K   � %               o%   o           %              
"   
 f�           d&    1� k   f� K   � %               o%   o           o%   o           
"   
 f�           �&    1� w   f� K   � %               o%   o           %              
"   
 f�           \'    1� �   f� K   � %               o%   o           o%   o           
"   
 n�           �'    1� �   n� K   � %               o%   o           %              
"   
 n�           T(    1� �   n� K   � %               o%   o           o%   o           
"   
 ��           �(    1� �   �� �  	 � %               o%   o           �     fP �L 
�H T   %              �     }        �GG %              
"   
 f�           �)    1� �   f� �   � %               o%   o           %               
"   
 f�           *    1� �   f� �   � %               o%   o           o%   o           
"   
 g�           �*    1� �   g�    � %               o%   o           �     f
"   
 f�           +    1� �   f�    � %               o%   o           � �  - g
"   
 f�           x+    1�    f�    � %               o%   o           �     f
"   
 n�           �+    1� 6   n�    � %               o%   o           � S   f
"   
 � �          `,    1� q   � � �     
"   
 f�           �,    1� �   f�    � %               o%   o           �     f
"   
 � �          -    1� �  
 � � �     
"   
 � �          L-    1� �   � � �     
"   
 g�           �-    1� �   g� �  	 � %               o%   o           �     f
"   
 f�           �-    1� �   f�    � %               o%   o           �     g
"   
 f�           p.    1� �   f� �   � %               o%   o           o%   o           
"   
 n�           �.    1� �   n�    � %               o%   o           � �  ! n
"   
 f�           `/    1�    f�    � %               o%   o           �     n
"   
 ��           �/    1�    ��    � %               o%   o           � "   f
"   
 ��           H0    1� 1  	 �� �   � %               o%   o           o%   o           
"   
 f�           �0    1� ;   f� �   � %               o%   o           %               
"   
 � �          @1    1� G   � � �     
"   
 f�           |1    1� U   f�    � %               o%   o           � i   f
"   
 n�           �1    1� x   n� �  	 � %               o%   o           �     f
"   
 n�           d2    1� �   n� �  	 � %               o%   o           �     n
"   
 � �          �2    1� �   � � �     
"   
 � �          3    1� �   � � �  	   
"   
 ��           P3    1� �   �� �   � o%   o           o%   o           %               
"   
 � �          �3    1� �   � � �     
"   
 � �          4    1� �   � � �  	   
"   
 � �          D4    1� �   � � �  	   
"   
 � �          �4    1� 	   � � �  	   
"   
 � �          �4    1�    � � �  	   
"   
 � �          �4    1� +   � � �  	   
"   
 � �          45    1� <   � � �     
"   
 n�           p5    1� M   n�    � %               o%   o           � d  4 f
"   
 � �          �5    1� �   � � �     
"   
 � �           6    1� �   � � �     
"   
 � �          \6    1� �   � � �     
"   
 � �          �6    1� �   � � �  	   
"   
 � �          �6    1� �   � � �  	   
"   
 � �          7    1� �   � � �  	   
"   
 � �          L7    1� �   � � �     
"   
 g�           �7    1�    g� �  	 � %               o%   o           �     f
"   
 f�           �7    1�    f� �  	 � %               o%   o           �     g
"   
 f�           p8    1� "   f� �  	 � %               o%   o           �     f
"   
 n�           �8    1� 7   n� �  	 � %               o%   o           �     f
"   
 f�           X9    1� L   f� �   � %               o%   o           %               
"   
 f�           �9    1� Z   f� �   � %               o%   o           o%   o           
"   
 n�           P:    1� l   n� �   � %               o%   o           %               
"   
 f�           �:    1� |   f� �   � %               o%   o           %               
"   
 f�           H;    1� �   f� �   � %               o%   o           o%   o           
"   
 f�           �;    1� �   f� �   � %               o%   o           %               
"   
 � �          @<    1� �   � � �  	   
"   
 f�           |<    1� �   f� �   � %               o%   o           %              
"   
 � �          �<    1� �   � � �  	   
"   
 � �          4=    1� �   � � �  	   
"   
 � �          p=    1� �  
 � � �  	   
"   
 f�           �=    1� �   f� �  	 � %               o%   o           � L   f
"   
 f�            >    1�    f� �  	 � %               o%   o           �     fP �L 
�H T   %              �     }        �GG %              
"   
 f�           �>    1�    f�    � %               o%   o           �     f
"   
 n�           \?    1� '   n� �   � %               o%   o           %               
"   
 n�           �?    1� 4   n�    � %               o%   o           �     n
"   
 f�     ,      L@    1� D   f�    � %               o%   o           �   � �     � T   o�    	 f
"   
 n�           �@    1� V   n� �   � %               o%   o           o%   o           
"   
 f�           \A    1� _   f�    � %               o%   o           �     g
"   
 f�           �A    1� m   f�    � %               o%   o           �     f
"   
 f�           DB    1� |   f� �  	 � %               o%   o           o%   o           
"   
 f�           �B    1� �   f�    � %               o%   o           o%   o           
"   
 ��           <C    1� �   ��    � %               o%   o           �     f
"   
 f�           �C    1� �   f� �   � %               o%   o           %               
"   
 � �          ,D    1� �   � � �     
"   
 g�           hD    1� �   g�    � %               o%   o           � �  ~ n
"   
 f�           �D    1� g    f�    � %               o%   o           �     g
"   
 f�           PE    1� y    f�    � %               o%   o           � �    f
"   
 f�           �E    1� �    f� �  	 � %               o%   o           � �    f
"   
 f�           8F    1� �    f� �  	 � %               o%   o           � �    f
"   
 n�           �F    1� �   	 n�    � %               o%   o           � �    f
"   
 f�            G    1� �   
 f� �  	 � %               o%   o           � �    n
"   
 f�           �G    1� �    f� �   � %               o%   o           o%   o           
"   
 g�           H    1� !   g�    � %               o%   o           � !   n
"   
 f�           �H    1� *!   f�    � %               o%   o           �     g
"   
 f�           �H    1� 3!  
 f� �   � %               o%   o           o%   o           
"   
 � �          tI    1� >!   � � �     
"   
 f�           �I    1� L!   f�    � %               o%   o           � `!  ] f
"   
 f�           $J    1� �!   f�    � %               o%   o           �     f
"   
 f�           �J    1� �!   f�    � %               o%   o           � �!   f
"   
 n�           K    1� �!   n� �   � %               o%   o           %               
"   
 f�           �K    1� �   f�    � %               o%   o           �     n
"   
 f�           �K    1� �!   f�    � %               o%   o           o%   o           
"   
 � �          xL    1� "   � � �  	   P �L 
�H T   %              �     }        �GG %              
"   
 f�           M    1� "   f� �   � %               o%   o           %               
"   
 f�           �M    1� &"  	 f� �   � %               o%   o           %               
"   
 � �           N    1� 0"   � �          
�    
�        �     }         �     }        �     }             �     }        �%                  �     }         �     }        �     }             �     }        �%              
�             �G "    � %     start-super-proc �� %     adm2/smart.p �oP �L 
�H T   %              �     }        �GG %              
"   
   �       �O    6� H     
"   
   
�        P    8
"   
   �        <P    ��     }        �G 4              
"   
 ߱G %              G %              %� � �   EnabledObjFldsToDisable,ModifyFields,DataSourceNames,UpdateTargetNames,LogicalObjectName,LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout  
�H T   %              �     }        �GG %              
"   
 o
"   
 � 
"   
 o
"   
   (�  L ( l       �        �Q    �� H   � P   �        �Q    �@    
� @  , 
�       �Q    �� Q   op�               �L
�    %              � 8       R    � $         � X          
�    � r   o
"   
 �p� @  , 
�       S    �� �   �p�               �L"    , �   � k"   n� m"   � �     }        �A      |    "      � k"   n%              (<   \ (    |    �     }        �A� o"   �A"  	  n    "    o"  	  n  < "    o"  	  n(    |    �     }        �A� o"   �A"  	  n
�H T   %              �     }        �GG %              
"   
 o
"   
 � 
"   
 o
"   
   (�  L ( l       �        �T    �� H   � P   �        �T    �@    
� @  , 
�       �T    �� Q   op�               �L
�    %              � 8      U    � $         � X          
�    � r   o
"   
 �p� @  , 
�       V    ��   
 �p�               �L"    , 
�H T   %              �     }        �GG %              
"   
 o
"   
 � 
"   
 o
"   
 n(�  L ( l       �        �V    �� H   � P   �        �V    �@    
� @  , 
�       �V    �� Q   op�               �L
�    %              � 8      �V    � $         � X   o     
�    � r   � 
"   
 �p� @  , 
�       �W    �� �   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
   
"   
  
"   
   
"   
   (�  L ( l       �        �X    �� H   � P   �        �X    �@    
� @  , 
�       �X    �� Q     p�               �L
�    %              � 8      �X    � $         � X          
�    � r     
"   
 �p� @  , 
�       �Y    �� ,  
 �p�               �L%     SmartDataViewer 
"   
   p� @  , 
�       8Z    �� G     p�               �L%      FRAME   
"   
  p� @  , 
�       �Z    �� J    p�               �L%               
"   
  p� @  , 
�       �Z    �� (    p�               �L(        �       �       �       �     }        �A
�H T   %              �     }        �GG %              
"   
 f (   � 
"   
 o    �        �[    �� H   �
"   
   � 8      $\    � $         � X          
�    � r   o
"   
   �        |\    �
"   
   �       �\    /
"   
   
"   
   �       �\    6� H     
"   
   
�        �\    8
"   
   �        ]    �
"   
   �       4]    �
"   
   p�    � �"   n
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 o    �        �]    �A"    �A
"   
   
�        D^    �@ � 
"   
 f"      �       }        �
"   
 � %              %                "    � %     start-super-proc �� %     adm2/appserver.p n�    � #     
�    �     }        �%               %      Server  - �     }        �    "  
  f�     � %                   "    f�     � %      NONE    p�,  8         $     "    n        � 3#   o
�    
�H T   %              �     }        �GG %              
"   
 o
"   
 � 
"   
 o
"   
   (�  L ( l       �        �`    �� H   � P   �        �`    �@    
� @  , 
�       �`    �� Q   op�               �L
�    %              � 8      �`    � $         � X          
�    � r   o
"   
 �p� @  , 
�       �a    �� �   �p�               �L"    , p�,  8         $     "  
  n        � A#   o
�     "    � %     start-super-proc �� %     adm2/visual.p o� 
"    
 � %     contextHelp 
"    
   
�    
�    %     processAction   
�    %     CTRL-PAGE-UP �o%     processAction   
�    %     CTRL-PAGE-DOWN  "    � %     start-super-proc �� %     adm2/containr.p %     modifyListProperty  
�    
�    %      Add     %      ContainerSourceEvents n%      initializeDataObjects n0 0   A    �    � �#   n
�    � �#   � A    �    � �#     
�    � �#   � %     modifyListProperty  
�    
�    %      Add     %      ContainerSourceEvents  %     buildDataRequest ent0 A    �    � �#   � 
�    � �#   f%     modifyListProperty  
�    
�    %      Add     %     SupportedLinks %      ContainerToolbar-Target %               
�H T   %              �     }        �GG %              
"   
 o
"   
 � 
"   
 o
"   
  (�  L ( l       �        @f    �� H   � P   �        Lf    �@    
� @  , 
�       Xf    �� Q   op�               �L
�    %              � 8      df    � $         � X   o     
�    � r   � 
"   
 �p� @  , 
�       tg    �� �   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
 o
"   
 � 
"   
 o
"   
 o(�  L ( l       �         h    �� H   � P   �        ,h    �@    
� @  , 
�       8h    �� Q   op�               �L
�    %              � 8      Dh    � $         � X   o     
�    � r   o
"   
 �p� @  , 
�       Ti    �� L   �p�               �L%               "    � %     start-super-proc �� %     adm2/datavis.p %     modifyListProperty  
�    %      ADD     %     SupportedLinks %     Toolbar-Target %     valueChanged    
�    %     valueChanged    
�     "    � %     start-super-proc �� %     adm2/viewer.p o%     modifyListProperty  
�    
�    %      Add     %     DataSourceEvents �f%     buildDataRequest �f�   � �   f� T     � }$  & o�   � �     � T   o� }$  & f�@    �    � �   o� �$   f     � �   o"    f� �   � �@    �    � �     � �$         � �   f"    � � �     
�H T   %              �     }        �GG %              
"   
 � 
"   
 o
"   
 � 
"   
 � (�  L ( l       �        �l    �� H   � P   �        �l    �@    
� @  , 
�       �l    �� Q   � p�               �L
�    %              � 8      �l    � $         � X   �      
�    � r     
"   
 �p� @  , 
�       n    �� 4   �p�               �L"    , 
"   
   p� @  , 
�       dn    �� _     p�               �L"    , 
"   
  p� @  , 
�       �n    �� 3!  
  p�               �L%               �             I%               �             �%              �     }        �
�                    �           x   `       ��                   %  �               4z         O   ����    e�          O   ����    R�          O   ����    ��         $     �   ���                       �[     
                    � ߱                   �          4   �����[                �                      ��                    $                  <�              (  �  �    0\               �  4          4   �����\                D                      ��                    #                  Ȃ              �  x  o         ,                                 �  �     �\      �  �     �\      �  $     �  ���                        ]     
                    � ߱          �      ]         �     @]      4  �     `]          $   "  `  ���                       �]  @         |]              � ߱                     (                T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           x   `       ��                 I  �  �               �         O   ����    e�          O   ����    R�          O   ����    ��      �"                      �          �  $   [  �   ���                       �]     
                    � ߱                  �  �                      ��                   \  ^                  ��p          \  (      4   ����^      $   ]  �  ���                       P^     
                    � ߱        d     _    (          4   ����d^      /  `  T                               3   ����x^  x  �   {  �^          O   �  ��  ��  �^               �          �  �   , �                          
                               �      ��                            ����                                                        x   `       ��                  �  �  �               ��f        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                                            �           x   `       ��                  2  >  �               X��        O   ����    e�          O   ����    R�          O   ����    ��      �      <  �� �                        =  �   �           4   ����lo      �   =  �o    ��                              ��        �                  ����                               �    d d     �   ��    � �                                               �      �                                                                  d     D                                                                 P   @d Kd                                                           �$  G   
 X @d �d                                                        !      �     g     �       P   @� Zd                                                           �$  G   
 X @� /d                                                       (      �  (   g     �        D                                                                    TXS appSrvUtils RowObject CodCia CodAlm Descripcion ASSIGNFOCUSEDWIDGET ASSIGNWIDGETVALUE ASSIGNWIDGETVALUELIST BLANKWIDGET CLEARWIDGET DISABLERADIOBUTTON DISABLEWIDGET ENABLERADIOBUTTON ENABLEWIDGET FORMATTEDWIDGETVALUE FORMATTEDWIDGETVALUELIST HIDEWIDGET HIGHLIGHTWIDGET RESETWIDGETVALUE TOGGLEWIDGET VIEWWIDGET WIDGETHANDLE WIDGETLONGCHARVALUE WIDGETISBLANK WIDGETISFOCUSED WIDGETISMODIFIED WIDGETISTRUE WIDGETVALUE WIDGETVALUELIST F-Main x(3) C�digo de almac�n X(40) Descripci�n de almac�n E:\OpenEdge\on_in_co\APLIC\vtablewi.w should only be RUN PERSISTENT. GETTARGETPROCEDURE GETOBJECTTYPE GETSHOWPOPUP SETSHOWPOPUP GETCREATEHANDLES GETDATAMODIFIED GETDISPLAYEDFIELDS GETDISPLAYEDTABLES GETENABLEDFIELDS GETENABLEDHANDLES GETFIELDHANDLES GETFIELDSENABLED GETGROUPASSIGNSOURCE GETGROUPASSIGNSOURCEEVENTS GETGROUPASSIGNTARGET GETGROUPASSIGNTARGETEVENTS GETNEWRECORD GETOBJECTPARENT GETRECORDSTATE GETROWIDENT GETTABLEIOSOURCE GETTABLEIOSOURCEEVENTS GETUPDATETARGET GETUPDATETARGETNAMES GETWINDOWTITLEFIELD OKTOCONTINUE SETCONTAINERMODE SETDATAMODIFIED SETDISPLAYEDFIELDS SETENABLEDFIELDS SETGROUPASSIGNSOURCE SETGROUPASSIGNSOURCEEVENTS SETGROUPASSIGNTARGET SETGROUPASSIGNTARGETEVENTS SETLOGICALOBJECTNAME SETOBJECTPARENT SETSAVESOURCE SETTABLEIOSOURCE SETTABLEIOSOURCEEVENTS SETUPDATETARGET SETUPDATETARGETNAMES SETWINDOWTITLEFIELD SHOWDATAMESSAGES DISABLEPAGESINFOLDER ENABLEPAGESINFOLDER GETCALLERPROCEDURE GETCALLERWINDOW GETCONTAINERMODE GETCONTAINERTARGET GETCONTAINERTARGETEVENTS GETCURRENTPAGE GETDISABLEDADDMODETABS GETDYNAMICSDOPROCEDURE GETFILTERSOURCE GETMULTIINSTANCEACTIVATED GETMULTIINSTANCESUPPORTED GETNAVIGATIONSOURCE GETNAVIGATIONSOURCEEVENTS GETNAVIGATIONTARGET GETOUTMESSAGETARGET GETPAGENTARGET GETPAGESOURCE GETPRIMARYSDOTARGET GETREENABLEDATALINKS GETRUNDOOPTIONS GETRUNMULTIPLE GETSAVEDCONTAINERMODE GETSDOFOREIGNFIELDS GETTOPONLY GETUPDATESOURCE GETWAITFOROBJECT GETWINDOWTITLEVIEWER GETSTATUSAREA PAGENTARGETS SETCALLEROBJECT SETCALLERPROCEDURE SETCALLERWINDOW SETCONTAINERTARGET SETCURRENTPAGE SETDISABLEDADDMODETABS SETDYNAMICSDOPROCEDURE SETFILTERSOURCE SETINMESSAGETARGET SETMULTIINSTANCEACTIVATED SETMULTIINSTANCESUPPORTED SETNAVIGATIONSOURCE SETNAVIGATIONSOURCEEVENTS SETNAVIGATIONTARGET SETOUTMESSAGETARGET SETPAGENTARGET SETPAGESOURCE SETPRIMARYSDOTARGET SETREENABLEDATALINKS SETROUTERTARGET SETRUNDOOPTIONS SETRUNMULTIPLE SETSAVEDCONTAINERMODE SETSDOFOREIGNFIELDS SETTOPONLY SETUPDATESOURCE SETWAITFOROBJECT SETWINDOWTITLEVIEWER SETSTATUSAREA GETALLFIELDHANDLES GETALLFIELDNAMES GETCOL GETDEFAULTLAYOUT GETDISABLEONINIT GETENABLEDOBJFLDS GETENABLEDOBJHDLS GETHEIGHT GETHIDEONINIT GETLAYOUTOPTIONS GETLAYOUTVARIABLE GETOBJECTENABLED GETOBJECTLAYOUT GETROW GETWIDTH GETRESIZEHORIZONTAL GETRESIZEVERTICAL SETALLFIELDHANDLES SETALLFIELDNAMES SETDEFAULTLAYOUT SETDISABLEONINIT SETHIDEONINIT SETLAYOUTOPTIONS SETOBJECTLAYOUT SETRESIZEHORIZONTAL SETRESIZEVERTICAL GETOBJECTTRANSLATED GETOBJECTSECURED CREATEUIEVENTS GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALVERSION SETOBJECTNAME SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE , prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDataViewer ContainerType FRAME PropertyDialog adm2/support/viewerd.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties EnabledObjFldsToDisable,ModifyFields,DataSourceNames,UpdateTargetNames,LogicalObjectName,LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout SupportedLinks Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext ObjectLayout LayoutOptions ObjectEnabled LayoutVariable DefaultLayout DisableOnInit EnabledObjFlds EnabledObjHdls FieldSecurity SecuredTokens AllFieldHandles AllFieldNames MinHeight MinWidth ResizeHorizontal ResizeVertical ObjectSecured ObjectTranslated PopupButtonsInFields ColorInfoBG INTEGER ColorInfoFG ColorWarnBG ColorWarnFG ColorErrorBG ColorErrorFG BGColor FGColor FieldPopupMapping CurrentPage PendingPage ContainerTarget ContainerTargetEvents exitObject,okObject,cancelObject,updateActive ContainerToolbarSource ContainerToolbarSourceEvents toolbar,okObject,cancelObject OutMessageTarget PageNTarget PageSource FilterSource UpdateSource UpdateTarget CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState StartPage RunMultiple WaitForObject DynamicSDOProcedure adm2/dyndata.w RunDOOptions InitialPageList WindowFrameHandle Page0LayoutManager MultiInstanceSupported MultiInstanceActivated ContainerMode SavedContainerMode SdoForeignFields NavigationSource NavigationTarget PrimarySdoTarget NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter CallerWindow CallerProcedure CallerObject DisabledAddModeTabs ReEnableDataLinks WindowTitleViewer UpdateActive InstanceNames ClientNames ContainedDataObjects ContainedAppServices DataContainer HasDbAwareObjects HasDynamicProxy HideOnClose HideChildContainersOnClose HasObjectMenu RequiredPages RemoveMenuOnHide ProcessList PageLayoutInfo PageTokens DataContainerName WidgetIDFileName CreateHandles DataModified DisplayedFields DisplayedTables   Editable EnabledFields EnabledHandles EnabledObjFldsToDisable EnabledWhenNew FieldHandles FieldsEnabled GroupAssignSource GroupAssignSourceEvents addRecord,copyRecord,updateRecord,resetRecord,undoRecord,cancelRecord,enableFields,disableFields,collectChanges,validateFields GroupAssignTarget GroupAssignTargetEvents updateState,LinkState InternalDisplayFromSource (Large) ModifyFields (All) NewRecord No ObjectMode View PrintPreviewActive RecordState NoRecordAvailable RowIdent SaveSource TableIOSource TableIOSourceEvents addRecord,updateRecord,copyRecord,deleteRecord,resetRecord,undoChange,cancelRecord,updateMode ToolbarSource ToolbarSourceEvents toolbar UndoNew UpdateTargetNames WindowTitleField KeepChildPositions ShowPopup FieldWidgetIDs ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService cFields adm2/visual.p CTRL-PAGE-UP CTRL-PAGE-DOWN adm2/containr.p Add initializeDataObjects getSupportedLinks data-target data-source buildDataRequest containertoolbar-target ContainerToolbar-Target adm2/datavis.p ADD Toolbar-Target DISPLAYOBJECTS cViewCols cEnabled iCol iEntries cEntry adm2/viewer.p RowObject.CodAlm RowObject.Descripcion ,RowObject. DISABLE_UI default Almac�n Descripci�n �  ("  �  �*      3 �    ��      0         pcFieldType     ��      T         pcColValues     ��      x         pcValue     ��      �         pcState �   ��      �         pcChanges       ��      �         pcChanges       ��               plCancel        ��      $        plAnswer        ��      H        plCancel        ��      l        pcRelative  �  ��      �        pcAction        ��      �        pcAction        ��      �        pcState     ��      �        pcReturn        ��              pcMode      ��      <        pcState     ��      \        pcNotValidFields    �  ��      �        pcProp      ��      �        pcProp      ��      �        plCancel    �  ��      �        pcProcName    ��             
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
 hTarget �  ��      �        pcMessage       ��      �        pcMessage       ��      �        plEnabled             	     cType       T	     T   �          D	                  getObjectType     )  +  �	        t	  
   hReposBuffer    �	        �	  
   hPropTable  �	        �	  
   hBuffer           �	  
   hTable  	  
     U   `	          
                  adm-clone-props                           "  #  $  %            t
  
   hProc             �
        pcProcName  �	  �
  	   V   `
  |
      �
                  start-super-proc    [  \  ]  ^  _  `  {  �  �  �
  8     W                                   4    l     X                                   8  9  <  �     Y                                   <  =  t  �     Z               �                  displayObjects  �  �        [                                   �  �  T     \                                   �  $  �     ]               �                  disable_UI  <  =  >  X    $    
 ,      �                          �  �     RowObject                               CodCia  CodAlm  Descripcion L          @  
   appSrvUtils t        `  
   gshAstraAppserver   �        �  
   gshSessionManager   �        �  
   gshRIManager    �        �  
   gshSecurityManager          �  
   gshProfileManager   <        $  
   gshRepositoryManager    h  	 	     P  
   gshTranslationManager   �  
 
     |  
   gshWebManager   �        �     gscSessionId    �        �     gsdSessionObj   �        �  
   gshFinManager             
   gshGenManager   @        0  
   gshAgnManager   d        T     gsdTempUniqueID �        x     gsdUserObj  �        �     gsdRenderTypeObj    �        �     gsdSessionScopeObj  �       �  
   ghProp           
   ghADMProps  4       $  
   ghADMPropsBuf   \       H     glADMLoadFromRepos  x       p     glADMOk �       �  
   ghContainer �       �     cObjectName �    	   �     iStart  �    
   �     cAppService             cASDivision @       (     cServerOperatingMode    \       T     cFields |       p     cViewCols   �       �     cEnabled    �       �     iCol    �       �     iEntries             �     cEntry        X    RowObject            D   ;  <  >  ?  �	  �	  �	  �	  �	  
  
  
  	
  
  
  
  
  
  
  
  
  
  
  
  
   
  !
  "
  %
  '
  (
  *
  +
  ,
  -
  .
  4
  6
  <
  >
  @
  A
  G
  H
  I
  J
  M
  N
  P
  Q
  S
  T
  U
  V
  W
  X
  Y
  [
  \
  ]
  _
  `
  a
  b
  c
  �
  K  L  O  P  Q  R  S  T  U  V  W  X  Y  Z  [  \  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  l  x  y  |  }  ~    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  *  8  9  ;  <  =  >  D  E  F  G  H  I  J  K  L  N  O  P  Q  R  S  T  U  V  W  X  Y  [  \  ^  _  `  a  �  �  �  �  �  �  V               "  *  c  d  m  n  r  s  t  v  y  �  �  �  �  �  �  	  �  �  �  �  �  �  �  �  �  �  �  �       G  �  �  1  2  3  5  7  ;  T  U  V  X  `  f  l  o  t  �  �    F  G  H  J  L  �  �  �  �  �  �              :%  C:\Progress\OpenEdge\src\adm2\viewer.i   �  �Q 2 %C:\Progress\OpenEdge\src\adm2\custom\viewercustom.i  �  } & C:\Progress\OpenEdge\src\adm2\datavis.i    � 1 %C:\Progress\OpenEdge\src\adm2\custom\dataviscustom.i H  f! ' C:\Progress\OpenEdge\src\adm2\containr.i �  � 0 %C:\Progress\OpenEdge\src\adm2\custom\containrcustom.i    �  �� ( C:\Progress\OpenEdge\src\adm2\visual.i      # / %C:\Progress\OpenEdge\src\adm2\custom\visualcustom.i  4  �< ) C:\Progress\OpenEdge\src\adm2\appserver.i    t  �� . %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   �  I� * C:\Progress\OpenEdge\src\adm2\smart.i    �  Ds - C:\Progress\OpenEdge\gui\fn  $  tw , %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   L  Q. + C:\Progress\OpenEdge\gui\set �  �/  C:\Progress\OpenEdge\src\adm2\viewprop.i �  �� $ %C:\Progress\OpenEdge\src\adm2\custom\viewpropcustom.i    �  ۃ % %C:\Progress\OpenEdge\src\adm2\custom\viewprtocustom.i    ,  ��  C:\Progress\OpenEdge\src\adm2\dvisprop.i p  B� " %C:\Progress\OpenEdge\src\adm2\custom\dvispropcustom.i    �  �� # %C:\Progress\OpenEdge\src\adm2\custom\dvisprtocustom.i    �  ��  C:\Progress\OpenEdge\src\adm2\cntnprop.i ,  ��   %C:\Progress\OpenEdge\src\adm2\custom\cntnpropcustom.i    `  P ! %C:\Progress\OpenEdge\src\adm2\custom\cntnprtocustom.i    �  F>  C:\Progress\OpenEdge\src\adm2\visprop.i  �  �I  %C:\Progress\OpenEdge\src\adm2\custom\vispropcustom.i   ��  %C:\Progress\OpenEdge\src\adm2\custom\visprtocustom.i \  �l  C:\Progress\OpenEdge\src\adm2\appsprop.i �  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    �  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i      i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i X  �j  C:\Progress\OpenEdge\gui\get �  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    �  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    �  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i <  Su  C:\Progress\OpenEdge\src\adm2\globals.i  p  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i �  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   �  �  C:\Progress\OpenEdge\src\adm2\appsprto.i (  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   \  �X  C:\Progress\OpenEdge\src\adm2\visprto.i  �  !�  %C:\Progress\OpenEdge\src\adm2\custom\visualdefscustom.i  �  n�  C:\Progress\OpenEdge\src\adm2\cntnprto.i    ;  %C:\Progress\OpenEdge\src\adm2\custom\containrdefscustom.i    P   �7 
 C:\Progress\OpenEdge\src\adm2\dvisprto.i �   0 	 %C:\Progress\OpenEdge\src\adm2\custom\datavisdefscustom.i �   ��  C:\Progress\OpenEdge\src\adm2\viewprto.i !  gf  %C:\Progress\OpenEdge\src\adm2\custom\viewerdefscustom.i  D!  ~�  C:\Progress\OpenEdge\src\adm2\widgetprto.i   �!  �h  E:\OpenEdge\on_in_co\aplic\dtables.i �!  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   �!  ��   E:\OpenEdge\on_in_co\APLIC\vtablewi.w        �   �      \"  �   �     l"     �  2   |"  �   z     �"     X  +   �"  �   U     �"     3  +   �"  �   2     �"       +   �"  \   �     �"  o   �  &   �"     P  1   #  U   6  &   #  �   /  '   ,#       +   <#  �     '   L#     �  +   \#  �   �  '   l#     �  0   |#  �   o  '   �#     m  -   �#  �   f  '   �#     d  -   �#  �   c  '   �#     a  -   �#  r   E  '   �#  n   -  (   �#     �  /   $  P   �  (   $  �   �  )   ,$     V  .   <$  �   Q  )   L$     /  +   \$  �   .  )   l$       +   |$  �   
  )   �$     �  +   �$  g   �  )   �$     �     �$  O   �  )   �$  �   !  *   �$       -   �$  �   �  *   �$     �  ,   %  �   �  *   %     j  +   ,%  �   i  *   <%     G  +   L%  �   F  *   \%     $  +   l%  �   #  *   |%       +   �%  �   �  *   �%     �  +   �%  �   �  *   �%     �  +   �%  }   �  *   �%     {  +   �%     �  *   �%     �  )   &     b  (   &     �  '   ,&     �  &   <&     _     L&  u   V     \&  O   H  $   l&     7  %   |&     �  $   �&  h   �     �&  �   �     �&  O   �  "   �&     �  #   �&     f  "   �&  {   3     �&  �   *     �&  O         '       !   '     �      ,'  �   u     <'  �   l     L'  O   ^     \'     M     l'     �     |'  �   �     �'  x   �     �'  M   �     �'     �     �'     `     �'  a   I     �'  �  (     �'     	     �'  �  �
     (  O   �
     (     �
     ,(     i
     <(  �   �	     L(     e     \(     �     l(  x   �     |(     �     �(     $     �(           �(          �(     �     �(  Q   �     �(     �     �(     Q     �(     =     )     #     )  f   �     ,)     �     <)  "   S     L)     ?     \)          l)  Z   �     |)     �     �)     �     �)     �     �)     h     �)  X   E     �)     �  
   �)      W     �)     C  	   �)     $     *  ]        *     �     ,*     �     <*     �     L*     o     \*     R     l*  0   �       |*     M      �*     *       �*     &      �*     !       �*           