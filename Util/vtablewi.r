	��VBvI�>    �              �                                 �� 3EF40143utf-8 MAIN O:\on_in_co\Util\vtablewi.w,, PROCEDURE disable_UI,, PROCEDURE displayObjects,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE startServerObject,, PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE restartServerObject,, PROCEDURE initializeServerObject,, PROCEDURE disconnectObject,, PROCEDURE destroyServerObject,, PROCEDURE bindServer,, PROCEDURE processAction,,INPUT pcAction CHARACTER PROCEDURE enableObject,, PROCEDURE disableObject,, PROCEDURE applyLayout,, PROCEDURE viewPage,,INPUT piPageNum INTEGER PROCEDURE viewObject,, PROCEDURE selectPage,,INPUT piPageNum INTEGER PROCEDURE removePageNTarget,,INPUT phTarget HANDLE,INPUT piPage INTEGER PROCEDURE passThrough,,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER PROCEDURE notifyPage,,INPUT pcProc CHARACTER PROCEDURE initPages,,INPUT pcPageList CHARACTER PROCEDURE initializeVisualContainer,, PROCEDURE hidePage,,INPUT piPageNum INTEGER PROCEDURE destroyObject,, PROCEDURE deletePage,,INPUT piPageNum INTEGER PROCEDURE createObjects,, PROCEDURE constructObject,,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE PROCEDURE changePage,, PROCEDURE assignPageProperty,,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER PROCEDURE validateFields,,INPUT-OUTPUT pcNotValidFields CHARACTER PROCEDURE updateTitle,, PROCEDURE updateRecord,, PROCEDURE updateMode,,INPUT pcMode CHARACTER PROCEDURE showDataMessagesProcedure,,OUTPUT pcReturn CHARACTER PROCEDURE resetRecord,, PROCEDURE queryPosition,,INPUT pcState CHARACTER PROCEDURE okToContinueProcedure,,INPUT pcAction CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE deleteRecord,, PROCEDURE dataAvailable,,INPUT pcRelative CHARACTER PROCEDURE confirmExit,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE confirmDelete,,INPUT-OUTPUT plAnswer LOGICAL PROCEDURE confirmContinue,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE collectChanges,,INPUT-OUTPUT pcChanges CHARACTER,INPUT-OUTPUT pcInfo CHARACTER PROCEDURE viewRecord,, PROCEDURE valueChanged,, PROCEDURE updateState,,INPUT pcState CHARACTER PROCEDURE toolbar,,INPUT pcValue CHARACTER PROCEDURE initializeObject,, PROCEDURE enableFields,, PROCEDURE displayFields,,INPUT pcColValues CHARACTER PROCEDURE disableFields,,INPUT pcFieldType CHARACTER PROCEDURE copyRecord,, PROCEDURE cancelRecord,, PROCEDURE addRecord,, FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION createUiEvents,LOGICAL, FUNCTION getObjectSecured,LOGICAL, FUNCTION getObjectTranslated,LOGICAL, FUNCTION setResizeVertical,LOGICAL,INPUT plResizeVertical LOGICAL FUNCTION setResizeHorizontal,LOGICAL,INPUT plResizeHorizontal LOGICAL FUNCTION setObjectLayout,LOGICAL,INPUT pcLayout CHARACTER FUNCTION setLayoutOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setHideOnInit,LOGICAL,INPUT plHide LOGICAL FUNCTION setDisableOnInit,LOGICAL,INPUT plDisable LOGICAL FUNCTION setDefaultLayout,LOGICAL,INPUT pcDefault CHARACTER FUNCTION setAllFieldNames,LOGICAL,INPUT pcValue CHARACTER FUNCTION setAllFieldHandles,LOGICAL,INPUT pcValue CHARACTER FUNCTION getResizeVertical,LOGICAL, FUNCTION getResizeHorizontal,LOGICAL, FUNCTION getWidth,DECIMAL, FUNCTION getRow,DECIMAL, FUNCTION getObjectLayout,CHARACTER, FUNCTION getObjectEnabled,LOGICAL, FUNCTION getLayoutVariable,CHARACTER, FUNCTION getLayoutOptions,CHARACTER, FUNCTION getHideOnInit,LOGICAL, FUNCTION getHeight,DECIMAL, FUNCTION getEnabledObjHdls,CHARACTER, FUNCTION getEnabledObjFlds,CHARACTER, FUNCTION getDisableOnInit,LOGICAL, FUNCTION getDefaultLayout,CHARACTER, FUNCTION getCol,DECIMAL, FUNCTION getAllFieldNames,CHARACTER, FUNCTION getAllFieldHandles,CHARACTER, FUNCTION setStatusArea,LOGICAL,INPUT plStatusArea LOGICAL FUNCTION setWindowTitleViewer,LOGICAL,INPUT phViewer HANDLE FUNCTION setWaitForObject,LOGICAL,INPUT phObject HANDLE FUNCTION setUpdateSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setTopOnly,LOGICAL,INPUT plTopOnly LOGICAL FUNCTION setSdoForeignFields,LOGICAL,INPUT cSdoForeignFields CHARACTER FUNCTION setSavedContainerMode,LOGICAL,INPUT cSavedContainerMode CHARACTER FUNCTION setRunMultiple,LOGICAL,INPUT plMultiple LOGICAL FUNCTION setRunDOOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setRouterTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setReEnableDataLinks,LOGICAL,INPUT cReEnableDataLinks CHARACTER FUNCTION setPrimarySdoTarget,LOGICAL,INPUT hPrimarySdoTarget HANDLE FUNCTION setPageSource,LOGICAL,INPUT phObject HANDLE FUNCTION setPageNTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setOutMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setNavigationTarget,LOGICAL,INPUT cTarget CHARACTER FUNCTION setNavigationSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setNavigationSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setMultiInstanceSupported,LOGICAL,INPUT lMultiInstanceSupported LOGICAL FUNCTION setMultiInstanceActivated,LOGICAL,INPUT lMultiInstanceActivated LOGICAL FUNCTION setInMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setFilterSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDynamicSDOProcedure,LOGICAL,INPUT pcProc CHARACTER FUNCTION setDisabledAddModeTabs,LOGICAL,INPUT cDisabledAddModeTabs CHARACTER FUNCTION setCurrentPage,LOGICAL,INPUT iPage INTEGER FUNCTION setContainerTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setCallerWindow,LOGICAL,INPUT h HANDLE FUNCTION setCallerProcedure,LOGICAL,INPUT h HANDLE FUNCTION setCallerObject,LOGICAL,INPUT h HANDLE FUNCTION pageNTargets,CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER FUNCTION getStatusArea,LOGICAL, FUNCTION getWindowTitleViewer,HANDLE, FUNCTION getWaitForObject,HANDLE, FUNCTION getUpdateSource,CHARACTER, FUNCTION getTopOnly,LOGICAL, FUNCTION getSdoForeignFields,CHARACTER, FUNCTION getSavedContainerMode,CHARACTER, FUNCTION getRunMultiple,LOGICAL, FUNCTION getRunDOOptions,CHARACTER, FUNCTION getReEnableDataLinks,CHARACTER, FUNCTION getPrimarySdoTarget,HANDLE, FUNCTION getPageSource,HANDLE, FUNCTION getPageNTarget,CHARACTER, FUNCTION getOutMessageTarget,HANDLE, FUNCTION getNavigationTarget,HANDLE, FUNCTION getNavigationSourceEvents,CHARACTER, FUNCTION getNavigationSource,CHARACTER, FUNCTION getMultiInstanceSupported,LOGICAL, FUNCTION getMultiInstanceActivated,LOGICAL, FUNCTION getFilterSource,HANDLE, FUNCTION getDynamicSDOProcedure,CHARACTER, FUNCTION getDisabledAddModeTabs,CHARACTER, FUNCTION getCurrentPage,INTEGER, FUNCTION getContainerTargetEvents,CHARACTER, FUNCTION getContainerTarget,CHARACTER, FUNCTION getContainerMode,CHARACTER, FUNCTION getCallerWindow,HANDLE, FUNCTION getCallerProcedure,HANDLE, FUNCTION enablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION disablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION showDataMessages,CHARACTER, FUNCTION setWindowTitleField,LOGICAL,INPUT cWindowTitleField CHARACTER FUNCTION setUpdateTargetNames,LOGICAL,INPUT pcTargetNames CHARACTER FUNCTION setUpdateTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setTableIOSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setTableIOSource,LOGICAL,INPUT phObject HANDLE FUNCTION setSaveSource,LOGICAL,INPUT plSave LOGICAL FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setLogicalObjectName,LOGICAL,INPUT pcLogicalObjectName CHARACTER FUNCTION setGroupAssignTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setGroupAssignTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setGroupAssignSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setGroupAssignSource,LOGICAL,INPUT phObject HANDLE FUNCTION setEnabledFields,LOGICAL,INPUT pcFieldList CHARACTER FUNCTION setDisplayedFields,LOGICAL,INPUT pcFieldList CHARACTER FUNCTION setDataModified,LOGICAL,INPUT plModified LOGICAL FUNCTION setContainerMode,LOGICAL,INPUT pcContainerMode CHARACTER FUNCTION okToContinue,LOGICAL,INPUT pcAction CHARACTER FUNCTION getWindowTitleField,CHARACTER, FUNCTION getUpdateTargetNames,CHARACTER, FUNCTION getUpdateTarget,CHARACTER, FUNCTION getTableIOSourceEvents,CHARACTER, FUNCTION getTableIOSource,HANDLE, FUNCTION getRowIdent,CHARACTER, FUNCTION getRecordState,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getNewRecord,CHARACTER, FUNCTION getGroupAssignTargetEvents,CHARACTER, FUNCTION getGroupAssignTarget,CHARACTER, FUNCTION getGroupAssignSourceEvents,CHARACTER, FUNCTION getGroupAssignSource,HANDLE, FUNCTION getFieldsEnabled,LOGICAL, FUNCTION getFieldHandles,CHARACTER, FUNCTION getEnabledHandles,CHARACTER, FUNCTION getEnabledFields,CHARACTER, FUNCTION getDisplayedTables,CHARACTER, FUNCTION getDisplayedFields,CHARACTER, FUNCTION getDataModified,LOGICAL, FUNCTION getCreateHandles,CHARACTER, FUNCTION setShowPopup,LOGICAL,INPUT plShowPopup LOGICAL FUNCTION getShowPopup,LOGICAL, FUNCTION getObjectType,character, FUNCTION getTargetProcedure,HANDLE, FUNCTION widgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION widgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION widgetIsTrue,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsModified,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetIsFocused,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsBlank,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetLongcharValue,LONGCHAR,INPUT pcName CHARACTER FUNCTION widgetHandle,HANDLE,INPUT pcName CHARACTER FUNCTION viewWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION toggleWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION resetWidgetValue,LOGICAL,INPUT pcNameList CHARACTER FUNCTION highlightWidget,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER FUNCTION hideWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION formattedWidgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION formattedWidgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION enableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION enableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION disableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION disableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION clearWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION blankWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION assignWidgetValueList,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION assignWidgetValue,LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER FUNCTION assignFocusedWidget,LOGICAL,INPUT pcName CHARACTER     �              |             �� �  ��              �o              +    +   u t  U   xy D  V   �| �   Z   �} d  ]            X  ? `� ,%  iSO8859-1                                                                           L    �                                      �                   ��                    p     �   J+   �             x�  �   �      �                                                         PROGRESS                                    
    
                    �              �                                                                                                     
  4         �          X  �          ��      0                       x                �   �  �      ,  
    
                    �             �                                                                                          �          
  \  �      �  
    
                  �  �             H                                                                                          �          
          �  
    
                  p  8             �                                                                                                    
  �        0  
    
                    �             �                                                                                                    
  `  .      �  
    
                  �  �             L                                                                                          .          
    @      �  
    
                  t  <             �                                                                                          @          
  �  U      4  
    
                     �  	           �                                                                                          U          
  d  k      �  
    
                  �  �  
           P                                                                                          k          
    y      �                         x  @             �                                                                                          y            �  �      8                        $  �             �                                                                                          �            h	  �      �  
    
                  �  �	             T	                                                                                          �          
  
  �      �	  
    
                  |	  D
              
                                                                                          �          
  �
  �      <
  
    
                  (
  �
             �
                                                                                          �          
  l  �      �
                        �
  �             X                                                                                          �              �      �                        �  H                                                                                                       �            �  �      @                        ,  �             �                                                                                          �                �      �                        �                 \                                                                                          �                          ��                                               ��          �  0  H X�            
             
             
                                         
                                                                                                                                                                        H   X   h   x   �   �   �   �   �   �   �   �       (  8  H      H   X   h   x   �   �   �   �   �   �   �   �      (  8  H                                                                                                                                                    x  �  �  �  �          �             �  �  �  �  �          �                    (            ,             D  L  T  d  \                         h  t  �  �                             �  �  �  �                             �  �  �  �                                                                         CodCta  X(10)   Cuenta contable Cuenta!contable     C�digo de cuenta contable   CodFam  X(4)    Codigo  Codigo      Codigo de familia   DesFam  X(40)   Descripci�n Descripci�n     Descripci�n de familia  PorDep  ZZ9.99  PorDep  PorDep  0   trg-FchAct  99/99/9999  Ultima Actualizaci�n    ?   trg-HraAct  X(8)    Hora Actualizaci�n      trg-Usuario X(11)   Usuario     �  ���	������    �  �       %                �     i    	 	       !   (   /   6   A   L     ��                                               �          ����                            undefined                                                               �           x   `                             �����               ��        O   ����    e�          O   ����    R�          O   ����    ��      d        �   �           4   ����     /     �                                3   ����       $      8  ���                       8      
                       � ߱        x  �      D       `
     H          assignFocusedWidget         �      �     X       LOGICAL,INPUT pcName CHARACTER  assignWidgetValue   �      �          l       LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER  assignWidgetValueList   �      L      �    ~       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER  blankWidget d      �          �       LOGICAL,INPUT pcNameList CHARACTER  clearWidget �      0      \    �       LOGICAL,INPUT pcNameList CHARACTER  disableRadioButton  <      �      �    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    disableWidget   �      �      $    �       LOGICAL,INPUT pcNameList CHARACTER  enableRadioButton         H      |    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    enableWidget    \      �      �    �       LOGICAL,INPUT pcNameList CHARACTER  formattedWidgetValue    �            H  	  �       CHARACTER,INPUT pcName CHARACTER    formattedWidgetValueList    (      l      �  
        CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    hideWidget  �      �         
       LOGICAL,INPUT pcNameList CHARACTER  highlightWidget �      <      l    %      LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER  resetWidgetValue    L      �      �    5      LOGICAL,INPUT pcNameList CHARACTER  toggleWidget    �            8    F      LOGICAL,INPUT pcNameList CHARACTER  viewWidget        \      �   
 S      LOGICAL,INPUT pcNameList CHARACTER  widgetHandle    h      �      �    ^      HANDLE,INPUT pcName CHARACTER   widgetLongcharValue �      �      0    k      LONGCHAR,INPUT pcName CHARACTER widgetIsBlank         P      �          LOGICAL,INPUT pcNameList CHARACTER  widgetIsFocused `      �      �    �      LOGICAL,INPUT pcName CHARACTER  widgetIsModified    �      �      (	    �      LOGICAL,INPUT pcNameList CHARACTER  widgetIsTrue    	      L	      |	    �      LOGICAL,INPUT pcName CHARACTER  widgetValue \	      �	      �	    �      CHARACTER,INPUT pcName CHARACTER    widgetValueList �	      �	      
    �      CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    `�     E  x
  �
          4   ����d                 �
                      ��                  E  I                  |           E  �
  <  	  F  ,                                        3   ����|       O   H  ��  ��  �   addRecord                               �  �      ��                  �  �  �              ��~        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            cancelRecord                                �  �      ��                  �  �  �              ȹ�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            copyRecord                              �  �      ��                  �  �  �              H��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableFields                               �  �      ��                  �     �              �        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            displayFields                               �  �      ��                      �              ���        O   ����    e�          O   ����    R�          O   ����    ��            ��                             ��                            ����                            enableFields                                �  �      ��                                    �I5        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �  �      ��                  	  
                �J5        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            toolbar                             �  �      ��                                     ��         O   ����    e�          O   ����    R�          O   ����    ��            ��                             ��                            ����                            updateState                               �      ��                                    <�q        O   ����    e�          O   ����    R�          O   ����    ��            ��                  4           ��                            ����                            valueChanged                                $        ��                      <               ��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewRecord                                       ��                      0              ���        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            getTargetProcedure  �	      �      �    m      HANDLE, getObjectType   �      �      �    �      CHARACTER,  getShowPopup    �             0    �      LOGICAL,    setShowPopup          <      l    �      LOGICAL,INPUT plShowPopup LOGICAL   addRecord                                 �      ��                  �  �  ,              0��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            cancelRecord                                  �      ��                  �  �  $              8��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            collectChanges                                �      ��                  �  �                p�r        O   ����    e�          O   ����    R�          O   ����    ��            ��   h             4               ��                  \           ��                            ����                            confirmContinue                             L  4      ��                  �  �  d              ໌        O   ����    e�          O   ����    R�          O   ����    ��            ��                  |           ��                            ����                            confirmDelete                               l  T      ��                  �  �  �              `��        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            confirmExit                             �  p      ��                  �  �  �              �f        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            copyRecord                              �  �      ��                  �  �  �              (6�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            dataAvailable                               �  �      ��                  �  �  �               7�        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            deleteRecord                                �   �       ��                  �  �  �               `�v        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �!  �!      ��                  �  �  �!              $�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            okToContinueProcedure                               �"  �"      ��                  �  �  �"              D�        O   ����    e�          O   ����    R�          O   ����    ��            ��   #             �"               ��                  #           ��                            ����                            queryPosition                                $  �#      ��                  �  �  $              xǑ        O   ����    e�          O   ����    R�          O   ����    ��            ��                  0$           ��                            ����                            resetRecord                             %  %      ��                  �  �  4%              �y        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            showDataMessagesProcedure                                &  &      ��                  �  �  8&               y        O   ����    e�          O   ����    R�          O   ����    ��            ��                  P&           ��                            ����                            updateMode                              <'  $'      ��                  �  �  T'              �y        O   ����    e�          O   ����    R�          O   ����    ��            ��                  l'           ��                            ����                            updateRecord                                \(  D(      ��                  �  �  t(              f        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateState                             P)  8)      ��                  �  �  h)              �f        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �)           ��                            ����                            updateTitle                             l*  T*      ��                  �  �  �*              T        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            validateFields                              d+  L+      ��                  �  �  |+              @	        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �+           ��                            ����                            getCreateHandles    L      �+      0,    �      CHARACTER,  getDataModified ,      <,      l,    �      LOGICAL,    getDisplayedFields  L,      x,      �,    �      CHARACTER,  getDisplayedTables  �,      �,      �,    �      CHARACTER,  getEnabledFields    �,      �,      ,-     �      CHARACTER,  getEnabledHandles   -      8-      l-  !         CHARACTER,  getFieldHandles L-      x-      �-  "        CHARACTER,  getFieldsEnabled    �-      �-      �-  #  "      LOGICAL,    getGroupAssignSource    �-      �-      ,.  $  3      HANDLE, getGroupAssignSourceEvents  .      4.      p.  %  H      CHARACTER,  getGroupAssignTarget    P.      |.      �.  &  c      CHARACTER,  getGroupAssignTargetEvents  �.      �.      �.  '  x      CHARACTER,  getNewRecord    �.      /      8/  (  �      CHARACTER,  getObjectParent /      D/      t/  )  �      HANDLE, getRecordState  T/      |/      �/  *  �      CHARACTER,  getRowIdent �/      �/      �/  +  �      CHARACTER,  getTableIOSource    �/      �/      $0  ,  �      HANDLE, getTableIOSourceEvents  0      ,0      d0  -  �      CHARACTER,  getUpdateTarget D0      p0      �0  .  �      CHARACTER,  getUpdateTargetNames    �0      �0      �0  /        CHARACTER,  getWindowTitleField �0      �0      $1  0        CHARACTER,  okToContinue    1      01      `1  1  ,      LOGICAL,INPUT pcAction CHARACTER    setContainerMode    @1      �1      �1  2  9      LOGICAL,INPUT pcContainerMode CHARACTER setDataModified �1      �1      2  3  J      LOGICAL,INPUT plModified LOGICAL    setDisplayedFields  �1      42      h2  4  Z      LOGICAL,INPUT pcFieldList CHARACTER setEnabledFields    H2      �2      �2  5  m      LOGICAL,INPUT pcFieldList CHARACTER setGroupAssignSource    �2      �2      3  6  ~      LOGICAL,INPUT phObject HANDLE   setGroupAssignSourceEvents  �2      <3      x3  7  �      LOGICAL,INPUT pcEvents CHARACTER    setGroupAssignTarget    X3      �3      �3  8  �      LOGICAL,INPUT pcObject CHARACTER    setGroupAssignTargetEvents  �3      �3      44  9  �      LOGICAL,INPUT pcEvents CHARACTER    setLogicalObjectName    4      X4      �4  :  �      LOGICAL,INPUT pcLogicalObjectName CHARACTER setObjectParent p4      �4      �4  ;  �      LOGICAL,INPUT phParent HANDLE   setSaveSource   �4      5      <5  <        LOGICAL,INPUT plSave LOGICAL    setTableIOSource    5      \5      �5  =        LOGICAL,INPUT phObject HANDLE   setTableIOSourceEvents  p5      �5      �5  >  "      LOGICAL,INPUT pcEvents CHARACTER    setUpdateTarget �5      6      <6  ?  9      LOGICAL,INPUT pcObject CHARACTER    setUpdateTargetNames    6      `6      �6  @  I      LOGICAL,INPUT pcTargetNames CHARACTER   setWindowTitleField x6      �6      �6  A  ^      LOGICAL,INPUT cWindowTitleField CHARACTER   showDataMessages    �6       7      T7  B  r      CHARACTER,  assignPageProperty                              �7  �7      ��                  �  �  8              .        O   ����    e�          O   ����    R�          O   ����    ��            ��   P8             8               ��                  D8           ��                            ����                            changePage                              09  9      ��                  �  �  H9              |�e        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmExit                             $:  :      ��                  �  �  <:              �@         O   ����    e�          O   ����    R�          O   ����    ��            ��                  T:           ��                            ����                            constructObject                             D;  ,;      ��                  �  �  \;              �E         O   ����    e�          O   ����    R�          O   ����    ��            ��   �;             t;               �� 
  �;             �;  
             ��   �;             �;               �� 
                 �;  
         ��                            ����                            createObjects                               �<  �<      ��                  �  �  �<              <`        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deletePage                              �=  �=      ��                      �=              �`        O   ����    e�          O   ����    R�          O   ����    ��            ��                   >           ��                            ����                            destroyObject                               �>  �>      ��                      ?              @gy        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hidePage                                �?  �?      ��                    
  �?              �gy        O   ����    e�          O   ����    R�          O   ����    ��            ��                  @           ��                            ����                            initializeObject                                A  �@      ��                       A              $hy        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeVisualContainer                               B  �A      ��                      $B              �U{        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initPages                                C  �B      ��                      C              �V{        O   ����    e�          O   ����    R�          O   ����    ��            ��                  0C           ��                            ����                            notifyPage                              D  D      ��                      4D              \��         O   ����    e�          O   ����    R�          O   ����    ��            ��                  LD           ��                            ����                            passThrough                             8E   E      ��                      PE              ��2        O   ����    e�          O   ����    R�          O   ����    ��            ��   �E             hE               ��                  �E           ��                            ����                            removePageNTarget                               �F  lF      ��                    "  �F              x�        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �F             �F  
             ��                  �F           ��                            ����                            selectPage                              �G  �G      ��                  $  &  �G              $Y        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �G           ��                            ����                            toolbar                             �H  �H      ��                  (  *  �H              �         O   ����    e�          O   ����    R�          O   ����    ��            ��                  I           ��                            ����                            viewObject                              �I  �I      ��                  ,  -  J              ܆         O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewPage                                �J  �J      ��                  /  1  K              �0+        O   ����    e�          O   ����    R�          O   ����    ��            ��                   K           ��                            ����                            disablePagesInFolder    47      �K      �K  C  �      LOGICAL,INPUT pcPageInformation CHARACTER   enablePagesInFolder �K      �K       L  D  �      LOGICAL,INPUT pcPageInformation CHARACTER   getCallerProcedure   L      LL      �L  E  �      HANDLE, getCallerWindow `L      �L      �L  F  �      HANDLE, getContainerMode    �L      �L      �L  G  �      CHARACTER,  getContainerTarget  �L       M      4M  H  �      CHARACTER,  getContainerTargetEvents    M      @M      |M  I  �      CHARACTER,  getCurrentPage  \M      �M      �M  J        INTEGER,    getDisabledAddModeTabs  �M      �M      �M  K        CHARACTER,  getDynamicSDOProcedure  �M      N      @N  L  2      CHARACTER,  getFilterSource  N      LN      |N  M  I      HANDLE, getMultiInstanceActivated   \N      �N      �N  N  Y      LOGICAL,    getMultiInstanceSupported   �N      �N      O  O  s      LOGICAL,    getNavigationSource �N      O      HO  P  �      CHARACTER,  getNavigationSourceEvents   (O      TO      �O  Q  �      CHARACTER,  getNavigationTarget pO      �O      �O  R  �      HANDLE, getOutMessageTarget �O      �O      P  S  �      HANDLE, getPageNTarget  �O      P      DP  T  �      CHARACTER,  getPageSource   $P      PP      �P  U  �      HANDLE, getPrimarySdoTarget `P      �P      �P  V         HANDLE, getReEnableDataLinks    �P      �P      �P  W        CHARACTER,  getRunDOOptions �P      Q      8Q  X  )      CHARACTER,  getRunMultiple  Q      DQ      tQ  Y  9      LOGICAL,    getSavedContainerMode   TQ      �Q      �Q  Z  H      CHARACTER,  getSdoForeignFields �Q      �Q      �Q  [  ^      CHARACTER,  getTopOnly  �Q      R      0R  \ 
 r      LOGICAL,    getUpdateSource R      <R      lR  ]  }      CHARACTER,  getWaitForObject    LR      xR      �R  ^  �      HANDLE, getWindowTitleViewer    �R      �R      �R  _  �      HANDLE, getStatusArea   �R      �R      $S  `  �      LOGICAL,    pageNTargets    S      0S      `S  a  �      CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER setCallerObject @S      �S      �S  b  �      LOGICAL,INPUT h HANDLE  setCallerProcedure  �S      �S      T  c  �      LOGICAL,INPUT h HANDLE  setCallerWindow �S      ,T      \T  d  �      LOGICAL,INPUT h HANDLE  setContainerTarget  <T      tT      �T  e        LOGICAL,INPUT pcObject CHARACTER    setCurrentPage  �T      �T      �T  f        LOGICAL,INPUT iPage INTEGER setDisabledAddModeTabs  �T      U      PU  g  #      LOGICAL,INPUT cDisabledAddModeTabs CHARACTER    setDynamicSDOProcedure  0U      �U      �U  h  :      LOGICAL,INPUT pcProc CHARACTER  setFilterSource �U      �U      V  i  Q      LOGICAL,INPUT phObject HANDLE   setInMessageTarget  �U      (V      \V  j  a      LOGICAL,INPUT phObject HANDLE   setMultiInstanceActivated   <V      |V      �V  k  t      LOGICAL,INPUT lMultiInstanceActivated LOGICAL   setMultiInstanceSupported   �V      �V      $W  l  �      LOGICAL,INPUT lMultiInstanceSupported LOGICAL   setNavigationSource W      TW      �W  m  �      LOGICAL,INPUT pcSource CHARACTER    setNavigationSourceEvents   hW      �W      �W  n  �      LOGICAL,INPUT pcEvents CHARACTER    setNavigationTarget �W      X      @X  o  �      LOGICAL,INPUT cTarget CHARACTER setOutMessageTarget  X      `X      �X  p  �      LOGICAL,INPUT phObject HANDLE   setPageNTarget  tX      �X      �X  q  �      LOGICAL,INPUT pcObject CHARACTER    setPageSource   �X      Y      8Y  r  	      LOGICAL,INPUT phObject HANDLE   setPrimarySdoTarget Y      XY      �Y  s  	      LOGICAL,INPUT hPrimarySdoTarget HANDLE  setReEnableDataLinks    lY      �Y      �Y  t  /	      LOGICAL,INPUT cReEnableDataLinks CHARACTER  setRouterTarget �Y      Z      HZ  u  D	      LOGICAL,INPUT phObject HANDLE   setRunDOOptions (Z      hZ      �Z  v  T	      LOGICAL,INPUT pcOptions CHARACTER   setRunMultiple  xZ      �Z      �Z  w  d	      LOGICAL,INPUT plMultiple LOGICAL    setSavedContainerMode   �Z      [      H[  x  s	      LOGICAL,INPUT cSavedContainerMode CHARACTER setSdoForeignFields ([      t[      �[  y  �	      LOGICAL,INPUT cSdoForeignFields CHARACTER   setTopOnly  �[      �[       \  z 
 �	      LOGICAL,INPUT plTopOnly LOGICAL setUpdateSource �[       \      P\  {  �	      LOGICAL,INPUT pcSource CHARACTER    setWaitForObject    0\      t\      �\  |  �	      LOGICAL,INPUT phObject HANDLE   setWindowTitleViewer    �\      �\       ]  }  �	      LOGICAL,INPUT phViewer HANDLE   setStatusArea   �\       ]      P]  ~  �	      LOGICAL,INPUT plStatusArea LOGICAL  applyLayout                             �]  �]      ��                  �  �  ^              ��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableObject                               �^  �^      ��                  �  �  _              \�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            enableObject                                �_  �_      ��                  �  �   `              X��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �`  �`      ��                  �  �  �`              ���        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            processAction                               �a  �a      ��                  �  �  �a               �e        O   ����    e�          O   ����    R�          O   ����    ��            ��                  b           ��                            ����                            getAllFieldHandles  0]      tb      �b    �	      CHARACTER,  getAllFieldNames    �b      �b      �b  �  �	      CHARACTER,  getCol  �b      �b      c  �  
      DECIMAL,    getDefaultLayout    �b      (c      \c  �  
      CHARACTER,  getDisableOnInit    <c      hc      �c  �  (
      LOGICAL,    getEnabledObjFlds   |c      �c      �c  �  9
      CHARACTER,  getEnabledObjHdls   �c      �c      d  �  K
      CHARACTER,  getHeight   �c      (d      Td  � 	 ]
      DECIMAL,    getHideOnInit   4d      `d      �d  �  g
      LOGICAL,    getLayoutOptions    pd      �d      �d  �  u
      CHARACTER,  getLayoutVariable   �d      �d      e  �  �
      CHARACTER,  getObjectEnabled    �d      e      Pe  �  �
      LOGICAL,    getObjectLayout 0e      \e      �e  �  �
      CHARACTER,  getRow  le      �e      �e  �  �
      DECIMAL,    getWidth    �e      �e      �e  �  �
      DECIMAL,    getResizeHorizontal �e      f      8f  �  �
      LOGICAL,    getResizeVertical   f      Df      xf  �  �
      LOGICAL,    setAllFieldHandles  Xf      �f      �f  �  �
      LOGICAL,INPUT pcValue CHARACTER setAllFieldNames    �f      �f      g  �        LOGICAL,INPUT pcValue CHARACTER setDefaultLayout    �f      ,g      `g  �        LOGICAL,INPUT pcDefault CHARACTER   setDisableOnInit    @g      �g      �g  �  $      LOGICAL,INPUT plDisable LOGICAL setHideOnInit   �g      �g      h  �  5      LOGICAL,INPUT plHide LOGICAL    setLayoutOptions    �g      (h      \h  �  C      LOGICAL,INPUT pcOptions CHARACTER   setObjectLayout <h      �h      �h  �  T      LOGICAL,INPUT pcLayout CHARACTER    setResizeHorizontal �h      �h      i  �  d      LOGICAL,INPUT plResizeHorizontal LOGICAL    setResizeVertical   �h      4i      hi  �  x      LOGICAL,INPUT plResizeVertical LOGICAL  getObjectTranslated Hi      �i      �i  �  �      LOGICAL,    getObjectSecured    �i      �i      j  �  �      LOGICAL,    createUiEvents  �i      j      @j  �  �      LOGICAL,    bindServer                              �j  �j      ��                  �  �  �j              \a1        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �k  �k      ��                  �  �  �k              �c1        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �l  �l      ��                  �  �  �l              Xg1        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �m  �m      ��                  �  �  �m              �g1        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �n  �n      ��                  �  �  �n              xh1        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �o  �o      ��                  �  �  �o              Xk1        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �p  �p      ��                  �  �  �p              l1        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �p  
         ��                            ����                            startServerObject                               �q  �q      ��                  �  �  �q              D4,        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                �r  �r      ��                  �  �  �r              (7,        O   ����    e�          O   ����    R�          O   ����    ��            ��                   s           ��                            ����                            getAppService    j      hs      �s  �  �      CHARACTER,  getASBound  xs      �s      �s  � 
 �      LOGICAL,    getAsDivision   �s      �s      t  �  �      CHARACTER,  getASHandle �s      t      Dt  �  �      HANDLE, getASHasStarted $t      Lt      |t  �  �      LOGICAL,    getASInfo   \t      �t      �t  � 	       CHARACTER,  getASInitializeOnRun    �t      �t      �t  �        LOGICAL,    getASUsePrompt  �t      u      4u  �         LOGICAL,    getServerFileName   u      @u      tu  �  /      CHARACTER,  getServerOperatingMode  Tu      �u      �u  �  A      CHARACTER,  runServerProcedure  �u      �u      �u  �  X      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   �u      <v      lv  �  k      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   Lv      �v      �v  �  y      LOGICAL,INPUT pcDivision CHARACTER  setASHandle �v      �v      w  �  �      LOGICAL,INPUT phASHandle HANDLE setASInfo   �v      4w      `w  � 	 �      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    @w      �w      �w  �  �      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �w      �w      x  �  �      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   �w      ,x      `x  �  �      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  @x      �x      �x  �  �      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             ly  Ty      ��                  {    �y              D'�        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �y             �y  
             ��   �y             �y               �� 
                 �y  
         ��                            ����                            addMessage                              �z  �z      ��                  �  �  �z              �q        O   ����    e�          O   ����    R�          O   ����    ��            ��   <{             {               ��   d{             0{               ��                  X{           ��                            ����                            adjustTabOrder                              H|  0|      ��                  �  �  `|              \B�        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �|             x|  
             �� 
  �|             �|  
             ��                  �|           ��                            ����                            applyEntry                              �}  �}      ��                  �  �  �}              �I�        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �}           ��                            ����                            changeCursor                                �~  �~      ��                  �  �  �~              ��        O   ����    e�          O   ����    R�          O   ����    ��            ��                             ��                            ����                            createControls                              �  �      ��                  �  �  �              d
�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �  Ԁ      ��                  �  �  �              \�        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �  ́      ��                  �  �  ��              �        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �  ̂      ��                  �  �  ��              ��        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              ؃  ��      ��                  �  �  ��              ,�4        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              ̄  ��      ��                  �  �  �              ��4        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                ȅ  ��      ��                  �  �  ��              ��4        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              Ć  ��      ��                  �  �  ܆              ��4        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  (�             �  
             ��   P�             �               ��   x�             D�               ��                  l�           ��                            ����                            modifyUserLinks                             \�  D�      ��                  �  �  t�              �,        O   ����    e�          O   ����    R�          O   ����    ��            ��   ��             ��               ��   �             ��               �� 
                 ܈  
         ��                            ����                            removeAllLinks                              ̉  ��      ��                  �  �  �              \7        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              ��  ��      ��                  �  �  ؊              �7        O   ����    e�          O   ����    R�          O   ����    ��            �� 
  $�             ��  
             ��   L�             �               �� 
                 @�  
         ��                            ����                            repositionObject                                4�  �      ��                  �  �  L�              d@        O   ����    e�          O   ����    R�          O   ����    ��            ��   ��             d�               ��                  ��           ��                            ����                            returnFocus                             x�  `�      ��                  �  �  ��              42        O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 ��  
         ��                            ����                            showMessageProcedure                                ��  ��      ��                  �  �  ��              x2        O   ����    e�          O   ����    R�          O   ����    ��            ��   �             Ў               ��                  ��           ��                            ����                            toggleData                              �  ̏      ��                  �  �  ��              `2        O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            viewObject                               �  �      ��                  �  �  �              ,2        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �x      p�      ��  � 
 8      LOGICAL,    assignLinkProperty  |�      ��      ܑ  �  C      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   ��      4�      d�  �  V      CHARACTER,  getChildDataKey D�      p�      ��  �  d      CHARACTER,  getContainerHandle  ��      ��      ��  �  t      HANDLE, getContainerHidden  ��      �      �  �  �      LOGICAL,    getContainerSource  ��      (�      \�  �  �      HANDLE, getContainerSourceEvents    <�      d�      ��  �  �      CHARACTER,  getContainerType    ��      ��      ��  �  �      CHARACTER,  getDataLinksEnabled ��      �       �  �  �      LOGICAL,    getDataSource    �      ,�      \�  �  �      HANDLE, getDataSourceEvents <�      d�      ��  �  �      CHARACTER,  getDataSourceNames  x�      ��      ؔ  �        CHARACTER,  getDataTarget   ��      �      �  �         CHARACTER,  getDataTargetEvents ��       �      T�  �  .      CHARACTER,  getDBAware  4�      `�      ��  � 
 B      LOGICAL,    getDesignDataObject l�      ��      ̕  �  M      CHARACTER,  getDynamicObject    ��      ؕ      �  �  a      LOGICAL,    getInstanceProperties   �      �      P�  �  r      CHARACTER,  getLogicalObjectName    0�      \�      ��  �  �      CHARACTER,  getLogicalVersion   t�      ��      Ԗ  �  �      CHARACTER,  getObjectHidden ��      ��      �  �  �      LOGICAL,    getObjectInitialized    �      �      T�  �  �      LOGICAL,    getObjectName   4�      `�      ��  �  �      CHARACTER,  getObjectPage   p�      ��      ̗  �  �      INTEGER,    getObjectVersion    ��      ؗ      �  �  �      CHARACTER,  getObjectVersionNumber  �      �      P�  �        CHARACTER,  getParentDataKey    0�      \�      ��  �        CHARACTER,  getPassThroughLinks p�      ��      И  �  )      CHARACTER,  getPhysicalObjectName   ��      ܘ      �  �  =      CHARACTER,  getPhysicalVersion  ��       �      T�  �  S      CHARACTER,  getPropertyDialog   4�      `�      ��  �  f      CHARACTER,  getQueryObject  t�      ��      Й  �  x      LOGICAL,    getRunAttribute ��      ܙ      �  �  �      CHARACTER,  getSupportedLinks   �      �      L�  �  �      CHARACTER,  getTranslatableProperties   ,�      X�      ��  �  �      CHARACTER,  getUIBMode  t�      ��      ̚  � 
 �      CHARACTER,  getUserProperty ��      ؚ      �  �  �      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    �      0�      h�  �  �      CHARACTER,INPUT pcPropList CHARACTER    linkHandles H�      ��      ��  �  �      CHARACTER,INPUT pcLink CHARACTER    linkProperty    ��      ��      �  �  �      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry �      L�      x�  �        CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   X�      �      �  �        CHARACTER,INPUT piMessage INTEGER   propertyType    ��      8�      h�  �  &      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  H�      ��      ��  �  3      CHARACTER,  setChildDataKey ��      ̝      ��  �  B      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  ܝ      $�      X�  �  R      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  8�      x�      ��  �  e      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    ��      ̞      �  �  x      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled �      ,�      `�  �  �      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   @�      ��      ��  �  �      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents ��      ؟      �  �  �      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  �      4�      h�  �  �      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   H�      ��      ��  �  �      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents ��      �      �  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  ��      <�      h�  � 
 �      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject H�      ��      ��  �        LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    ��      �      �  �        LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   ��      4�      l�  �  ,      LOGICAL,INPUT pcPropList CHARACTER  setLogicalVersion   L�      ��      Ģ  �  B      LOGICAL,INPUT cVersion CHARACTER    setObjectName   ��      �      �  �  T      LOGICAL,INPUT pcName CHARACTER  setObjectVersion    ��      8�      l�  �  b      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    L�      ��      ȣ  �  s      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks ��      �      $�  �  �      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   �      D�      |�  �  �      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  \�      ��      Ф  �  �      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute ��      ��      $�  �  �      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   �      L�      ��  �  �      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   `�      ��      �  �  �      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  ��      �      0�  � 
 �      LOGICAL,INPUT pcMode CHARACTER  setUserProperty �      P�      ��  �        LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage `�      ��      �  �        LOGICAL,INPUT pcMessage CHARACTER   Signature   ̦      �      <�  � 	 $      CHARACTER,INPUT pcName CHARACTER    �     �	  x�  �          4   �����                 ��                      ��                  �	  
                  ��*           �	  ��         �	  �  ��          4   �����                 ��                      ��                  �	  
                  X�*           �	   �  ��     
  ��  �          4   �����                 (�                      ��                  
  
                  ��*           
  ��         
                                  d     
                    � ߱        ��  $   
  T�  ���                           $   
  ة  ���                       �                         � ߱        ��     
  �  ��          4   �����                ��                      ��                  
  �
                  �*           
  ,�  Ъ  o   
      ,                                 (�  $    
  ��  ���                       4  @                        � ߱        <�  �   !
  T      P�  �   "
  �      d�  �   $
  <      x�  �   &
  �      ��  �   (
  $      ��  �   *
  �      ��  �   +
        ȫ  �   ,
  P      ܫ  �   /
  �      �  �   1
  8      �  �   2
  �      �  �   4
  0      ,�  �   5
  �      @�  �   6
  �      T�  �   7
  d      h�  �   8
  �      |�  �   >
  	      ��  �   @
  �	      ��  �   F
  �	      ��  �   H
  8
      ̬  �   J
  �
      �  �   K
  (      ��  �   Q
  �      �  �   R
        �  �   S
  �      0�  �   T
        D�  �   W
  |      X�  �   X
  �      l�  �   Z
  ,      ��  �   [
  h      ��  �   ]
  �      ��  �   ^
        ��  �   _
  T      Э  �   `
  �      �  �   a
  �      ��  �   b
  H      �  �   c
  �       �  �   e
  �      4�  �   f
  �      H�  �   g
  8      \�  �   i
  t      p�  �   j
  �      ��  �   k
  �      ��  �   l
  (          �   m
  d                      ��          $�  �      ��                    5  <�              ��*        O   ����    e�          O   ����    R�          O   ����    ��      �     
                P                     `                         � ߱        �  $    T�  ���                           O   3  ��  ��  �               P�          @�  H�    0�                                             ��                            ����                                �      ��      ��     T     X�                       T�  �                     ��     U  �  |�          4   �����                ��                      ��                  V  �                  �y           V  �  ��  �   Y        ��  �   Z  �      ȱ  �   [  �      ܱ  �   \  x      �  �   ]  �      �  �   ^  p      �  �   _  �      ,�  �   `  `      @�  �   a  �      T�  �   b  X      h�  �   c  �      |�  �   d  H      ��  �   e  �          �   f  @      l�     �  ��  ,�          4   �����                <�                      ��                  �  v                  �y           �  ̲  P�  �   �        d�  �   �  �      x�  �   �  �      ��  �   �  t      ��  �   �  �      ��  �   �  \      ȳ  �   �  �      ܳ  �   �  L      �  �   �  �      �  �   �  4       �  �   �  �       ,�  �   �  $!      @�  �   �  �!      T�  �   �  "      h�  �   �  �"      |�  �   �  #      ��  �   �  �#      ��  �   �  $      ��  �   �  �$      ̴  �   �  �$      �  �   �  x%      ��  �   �  �%      �  �      p&      �  �     �&      0�  �     h'      D�  �     �'      X�  �     `(          �     �(      x�     �  ��  ��          4   ����D)                �                      ��                  �  4                  d��            �  ��  �  �   �  �)      ,�  �   �   *      @�  �   �  �*      T�  �   �  +      h�  �   �  �+      |�  �   �  �+      ��  �   �  l,      ��  �   �  �,      ��  �   �  -      ̶  �   �  X-      �  �   �  �-      ��  �   �  .      �  �   �  |.      �  �   �  �.      0�  �   �  l/      D�  �   �  �/      X�  �   �  T0      l�  �   �  �0      ��  �   �  L1      ��  �   �  �1      ��  �   �  �1      ��  �   �  p2      з  �   �  �2      �  �   �   3      ��  �   �  \3      �  �   �  �3       �  �   �  4      4�  �   �  P4      H�  �   �  �4      \�  �   �  �4      p�  �   �  5      ��  �   �  @5      ��  �   �  |5      ��  �   �  �5      ��  �   �  ,6      Ը  �   �  h6      �  �   �  �6      ��  �   �  �6      �  �   �  7      $�  �   �  X7      8�  �   �  �7      L�  �   �  8      `�  �   �  |8      t�  �   �  �8      ��  �   �  d9      ��  �   �  �9      ��  �   �  \:      Ĺ  �   �  �:      ع  �   �  T;      �  �   �  �;       �  �   �  L<      �  �   �  �<      (�  �   �  =      <�  �   �  @=      P�  �   �  |=      d�  �   �  �=          �   �  ,>      |�     B  ��   �          4   �����>  	              �                      ��             	     C  �                  䇞           C  ��  $�  �   E  �>      8�  �   F  h?      L�  �   G  �?      `�  �   H  X@      t�  �   N  �@      ��  �   O  hA      ��  �   P  �A      ��  �   Q  PB      Ļ  �   R  �B      ػ  �   S  HC      �  �   T  �C       �  �   U  8D      �  �   V  tD      (�  �   X  �D      <�  �   Y  \E      P�  �   Z  �E      d�  �   [  DF      x�  �   \  �F      ��  �   ]  ,G      ��  �   ^  �G      ��  �   _  H      ȼ  �   `  �H      ܼ  �   a  I      �  �   b  �I      �  �   c  �I      �  �   e  0J      ,�  �   f  �J      @�  �   h  K      T�  �   i  �K      h�  �   j  L          �   k  �L      P�     �  ��  �          4   �����L  
              �                      ��             
     �  `                  H��           �  ��  (�  �   �  M      <�  �   �  �M          �   �  N      �     "  h�  ؾ          4   ����<N                �                      ��                  #  ,                  8%�           #  x�  h�     %   �  �          4   ����TN      $  &  <�  ���                       �N  @         �N              � ߱               )  ��  ��          4   �����N      $  *  ��  ���                       O  @         �N              � ߱        @�  $   4  �  ���                       <O     
                    � ߱        ��     m  X�  h�          4   ����PO      /   n  ��     ��                          3   ����`O            ��                      3   �����O  �     w  ��  \�  <�      4   �����O                l�                      ��                  x  �                  ��           x  ��  ��  �   |  �O      ��  $   }  ��  ���                       (P     
                    � ߱        ��  �   ~  HP      D�  $   �  �  ���                       pP  @         \P              � ߱         �  $   �  p�  ���                       �P                         � ߱        �Q     
                R                     \S  @        
 S              � ߱        ��  V   �  ��  ���                        hS                     �S       	       	       �S                         � ߱         �  $   �  ,�  ���                       �T     
                U                     dV  @        
 $V              � ߱        ��  V   �  ��  ���                        pV     
                �V                     <X  @        
 �W              � ߱            V   �  L�  ���                                      �                      ��                  �  �                  ��           �  ��  PX     
                �X                     Z  @        
 �Y          �Z  @        
 DZ          �Z  @        
 �Z          D[  @        
 [              � ߱            V     L�  ���                        adm-clone-props ��  0�              �     U     4                          0  �"                     start-super-proc    @�  ��  �           �     V                                  #                     ��     �  $�  4�          4   �����^      /   �  `�     p�                          3   �����^            ��                      3   ���� _  ��  $   �  ��  ���                        _       
       
           � ߱        ��     �  �  ��   �      4   ����<_                ��                      ��                  �  �                  �b�           �   �  P_       
       
       d_                     x_                         � ߱            $   �  ��  ���                              �  8�  t�          4   �����_  �_       
       
           � ߱            $   �  H�  ���                       ��     �  ��  ��   �      4   �����_      $   �  ��  ���                       �_                         � ߱            �   
  �_      8`     
                �`                     b  @        
 �a              � ߱        ��  V     4�  ���                        ��  �   Q  b      l�     �  ��   �          4   ����Pb      /   �  ,�     <�                          3   ����`b            \�                      3   �����b  0�     ;  ��  ��          4   �����b                �                      ��                  <  ?                  LP           <  ��      g   =  �         ���                           ��          ��  ��      ��                  >      ��              �P        O   ����    e�          O   ����    R�          O   ����    ��          /  >  �     �  �b                      3   �����b  D�     
   4�                      3   �����b         
   d�                      3   �����b    ��                              ��        �                  ����                                        0�              W      t�                      g                               ,�  g   A  H�          �	��                           �          ��  ��      ��                  A  C  ��              PS        O   ����    e�          O   ����    R�          O   ����    ��          /  B  0�     @�  �b                      3   �����b            `�                      3   ����c    ��                              ��        �                  ����                                        \�              X      p�                      g                               (�  g   E  D�          �	��                            �          ��  ��      ��                  E  G  ��              �h�        O   ����    e�          O   ����    R�          O   ����    ��          /  F  ,�     <�  <c                      3   ���� c            \�                      3   ����Dc    ��                              ��        �                  ����                                        X�              Y      l�                      g                               p�     ^  @�  ��          4   ����`c                ��                      ��                  _  ~                  ti�           _  P�  ,�  /   `  ��     ��                          3   ����pc            �                      3   �����c  (�  /  b  X�     h�  �c                      3   �����c  ��     
   ��                      3   �����c  ��        ��                      3   �����c  ��        ��                      3   �����c            �                      3   ����d  L�     j  @�  P�          4   ����8d      /  p  |�     ��  �d                      3   �����d  ��     
   ��                      3   �����d  ��        ��                      3   �����d  �        �                      3   �����d            <�                      3   ����e         v  d�  t�          4   ����(e      /  y  ��     ��  |e                      3   ����\e  ��     
   ��                      3   �����e  �         �                      3   �����e  @�        0�                      3   �����e            `�                      3   �����e  �     �  �e                                     �e     
                pf                     �g  @        
 �g              � ߱        ��  V   �  ��  ���                        �g     
                Ph                     �i  @        
 `i              � ߱        ��  V     4�  ���                        0�     P  ��  L�          4   �����i                \�                      ��                  Q  V                  �r           Q  ��  ��  /   R  ��     ��                          3   �����i            ��                      3   �����i      /   T  ��     �                          3   ���� j  4�     
   $�                      3   ���� j  d�        T�                      3   ����(j  ��        ��                      3   ����<j            ��                      3   ����Xj  displayObjects  ��  ��                      Z      �                               r$                     ��  g   �  H�         4��                           �          ��  ��      ��                  �      ��              �:�        O   ����    e�          O   ����    R�          O   ����    ��          /  �  0�         �j                      3   ����tj    ��                              ��        �                  ����                                        \�              [      @�                      g                               ��  g     �          0L�      }                      ��          ��  ��      ��                        ��              8;�        O   ����    e�          O   ����    R�          O   ����    ��          /    ��         �j                      3   �����j    ��                            ����                                        (�              \      �                      g                               (�       ��  0�          4   �����j                @�                      ��                                      �;�             ��  ��  /     l�     |�                          3   �����j            ��                      3   �����j      /    ��     ��  (k                      3   ����k  �     
   �                      3   ����0k  H�        8�                      3   ����8k  x�        h�                      3   ����Lk            ��                      3   ����lk  �k                     �k                     �k                     8l                         � ߱        ��  $     ��  ���                       �l     
                m                     Xn  @        
 n          �n  @        
 pn          o  @        
 �n              � ߱        d�  V   "  T�  ���                        0o  @         o          Xo  @         Do              � ߱            $     �  ���                       disable_UI  ��  ��                      ]                                    �$  
                    �  �   ���  �                $�  0�      toggleData  ,INPUT plEnabled LOGICAL    �  \�  t�      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  L�  ��  ��      returnFocus ,INPUT hTarget HANDLE   ��  ��   �      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    ��  <�  H�      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ,�  ��  ��      removeAllLinks  ,   ��  ��  ��      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE ��  (�  <�      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    �  ��  ��      hideObject  ,   ��  ��  ��      exitObject  ,   ��  ��  �      editInstanceProperties  ,   ��   �  0�      displayLinks    ,   �  D�  T�      createControls  ,   4�  h�  x�      changeCursor    ,INPUT pcCursor CHARACTER   X�  ��  ��      applyEntry  ,INPUT pcField CHARACTER    ��  ��  ��      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER ��  D�  P�      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER 4�  ��  ��      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  �  �      unbindServer    ,INPUT pcMode CHARACTER ��  <�  P�      startServerObject   ,   ,�  d�  t�      runServerObject ,INPUT phAppService HANDLE  T�  ��  ��      restartServerObject ,   ��  ��  ��      initializeServerObject  ,   ��  ��  �      disconnectObject    ,   ��  �  0�      destroyServerObject ,   �  D�  P�      bindServer  ,   4�  d�  t�      processAction   ,INPUT pcAction CHARACTER   T�  ��  ��      enableObject    ,   ��  ��  ��      disableObject   ,   ��  ��  ��      applyLayout ,   ��  �  �      viewPage    ,INPUT piPageNum INTEGER    ��  @�  L�      viewObject  ,   0�  `�  l�      selectPage  ,INPUT piPageNum INTEGER    P�  ��  ��      removePageNTarget   ,INPUT phTarget HANDLE,INPUT piPage INTEGER ��  ��  ��      passThrough ,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER  ��  <�  H�      notifyPage  ,INPUT pcProc CHARACTER ,�  p�  |�      initPages   ,INPUT pcPageList CHARACTER `�  ��  ��      initializeVisualContainer   ,   ��  ��  ��      hidePage    ,INPUT piPageNum INTEGER    ��  �   �      destroyObject   ,    �  4�  @�      deletePage  ,INPUT piPageNum INTEGER    $�  l�  |�      createObjects   ,   \�  ��  ��      constructObject ,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE ��  �   �      changePage  ,   �  4�  H�      assignPageProperty  ,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER $�  ��  ��      validateFields  ,INPUT-OUTPUT pcNotValidFields CHARACTER    x�  ��  ��      updateTitle ,   ��  ��  �      updateRecord    ,   ��  �  $�      updateMode  ,INPUT pcMode CHARACTER �  L�  h�      showDataMessagesProcedure   ,OUTPUT pcReturn CHARACTER  <�  ��  ��      resetRecord ,   ��  ��  ��      queryPosition   ,INPUT pcState CHARACTER    ��  ��  �      okToContinueProcedure   ,INPUT pcAction CHARACTER,OUTPUT plAnswer LOGICAL   ��  L�  \�      deleteRecord    ,   <�  p�  ��      dataAvailable   ,INPUT pcRelative CHARACTER `�  ��  ��      confirmExit ,INPUT-OUTPUT plCancel LOGICAL  ��  ��  ��      confirmDelete   ,INPUT-OUTPUT plAnswer LOGICAL  ��  (�  8�      confirmContinue ,INPUT-OUTPUT plCancel LOGICAL  �  h�  x�      collectChanges  ,INPUT-OUTPUT pcChanges CHARACTER,INPUT-OUTPUT pcInfo CHARACTER X�  ��  ��      viewRecord  ,   ��  ��  ��      valueChanged    ,   ��  �  �      updateState ,INPUT pcState CHARACTER    ��  D�  L�      toolbar ,INPUT pcValue CHARACTER    4�  x�  ��      initializeObject    ,   h�  ��  ��      enableFields    ,   ��  ��  ��      displayFields   ,INPUT pcColValues CHARACTER    ��  �  �      disableFields   ,INPUT pcFieldType CHARACTER    ��  D�  P�      copyRecord  ,   4�  d�  t�      cancelRecord    ,   T�  ��  ��      addRecord   ,        � 
"     
 %     adecomm/as-utils.w  
"   
   �    }        �
"     
    �     }        �� 2  :   %               � 
"    
 � %              � �  �         `      $              
�    � .   �      
�             �G                      
�            � 0   � 
"    
 
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �            7%               
"   
 �           H    1� @  
 � K   � %               o%   o           � P    
"   
 �           �    1� Q   � K   � %               o%   o           � _   
"   
 �           0    1� f  
 � K   � %               o%   o           � q   
"   
 �           �    1� �   � K   � %               o%   o           � �   
"   
 �               1� �   � K   � %               o%   o           � �   
"   
 �           �    1� �   � �   � %               o%   o           %               
"   
 � �              1� �   � � �     
"   
 �           D    1� �   � K   � %               o%   o           � �  � 
"   
 �           �    1� �   � K   � %               o%   o           � �  N 
"   
 �           ,    1�    � �   � %               o%   o           %               
"   
 �           �    1� &   � �   � %               o%   o           %               
"   
 �           $    1� 8   � �   � %               o%   o           %              
"   
 � �          �    1� E   � � �     
"   
 �           �    1� T  
 � �   � %               o%   o           %               
"   
 �           X    1� _   � K   � %               o%   o           � P    
"   
 � �          �    1� g   � � �     
"   
 �           	    1� w   � K   � %               o%   o           � �  t 
"   
 � �          |	    1�   
 � � �     
"   
 �           �	    1�    � K   � %               o%   o           �   � 
"   
 �           ,
    1� �   � K   � %               o%   o           � P    
"   
 �           �
    1� �  
 � �   � %               o%   o           %               
"   
 x�               1� �   x� �   � %               o%   o           %               
"   
 �           �    1� �   � K   � %               o%   o           � P    x
"   
 �               1� �   � K   � %               o%   o           o%   o           
"   
 *�           �    1� �  
 *� K   � %               o%   o           � P    x
"   
 �           �    1�    �   	 � %               o%   o           �    / *
"   
 � �          p    1� P   � �   	   
"   
 x�           �    1� b   x�   	 � o%   o           o%   o           � P    x
"   
 � �               1� u   � �   	   
"   
 x�           \    1� �   x�   	 � o%   o           o%   o           � P    x
"   
 � �          �    1� �   � � �     
"   
 � �              1� �   � �   	   
"   
 � �          H    1� �   � �   	   
"   
 � �          �    1� �   � �   	   
"   
 ~�           �    1� �   ~� �   � o%   o           o%   o           %              
"   
 � �          <    1� �   � �   	   
"   
 � �          x    1� �  
 � � �     
"   
 � �          �    1� �   � �   	   
"   
 � �          �    1�    � �   	   
"   
 � �          ,    1�    � �   	   
"   
 � �          h    1� 3   � �   	   
"   
 � �          �    1� B  	 � �   	   
"   
 � �          �    1� L   � �   	   
"   
 � �              1� _   � �   	   
"   
 �           X    1� v   � K   � %               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 
"   
   
"   
 �(�  L ( l       �             �� �   � P   �        ,    �@    
� @  , 
�       8    �� �     p�               �L
�    %              � 8      D    � $         � �          
�    � �     
"   
 �� @  , 
�       T    �� f  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 �                1� �  
 � K   � %               o%   o           � P    
"   
 �           t    1� �  
 � K   � %               o%   o           o%   o           
"   
 �           �    1� �   � �   � %               o%   o           o%   o           
"   
 �           l    1� �   � �   � %               o%   o           %               
"   
 x�           �    1� �   x� �   � %               o%   o           %               
"   
  �           d    1� �    � K   � %               o%   o           � P    x
"   
 ~�           �    1� �   ~� �   � %               o%   o           %              
"   
 ~�           T    1�    ~� �   � %               o%   o           o%   o           
"   
 *�           �    1�    *� K   � %               o%   o           o%   o           
"   
 �           L    1�   	 � K   � %               o%   o           � P    x
"   
 �           �    1� '   � K   � %               o%   o           o%   o           
"   
 y�           <    1� ;   y� K   � %               o%   o           o%   o           
"   
 x�           �    1� J   x� �   � %               o%   o           %               
"   
 x�           4    1� Z   x� �   � %               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 �               1� f   �   	 � %               o%   o           � P    
"   
 *�           x    1� s   *�   	 � %               o%   o           � P    
"   
 �           �    1� �   � �   � %               o%   o           %               
"   
  �           h    1� �    �   	 � %               o%   o           � P    
"   
 y�           �    1� �   y�   	 � %               o%   o           � P     
"   
 ~�           P    1� �   ~� �   � %               o%   o           %               
"   
 �           �    1� �   �   	 � %               o%   o           � P    ~
"   
 �           @    1� �   �   	 � %               o%   o           � P    
"   
 �           �    1� �   �   	 � %               o%   o           � P    
"   
 �           (     1� �   �   	 � %               o%   o           o%   o           
"   
 �           �     1� �   �   	 � %               o%   o           � P    *
"   
  �           !    1�     �   	 � %               o%   o           � P    
"   
 y�           �!    1�   	 y� �   � %               o%   o           %               
"   
 ~�           "    1�    ~� �   � %               o%   o           %               
"   
 ~�           �"    1� %   ~� �   � %               o%   o           o%   o           
"   
 �            #    1� 6   � �   � %               o%   o           o%   o           
"   
 �           |#    1� E   � �   � %               o%   o           %               
"   
 *�           �#    1� S   *� �   � %               o%   o           %               
"   
 �           t$    1� d   � �   � %               o%   o           %               
"   
  �           �$    1� y    � �   � %               o%   o           %       
       
"   
  �           l%    1� �    � �   � %               o%   o           o%   o           
"   
 x�           �%    1� �   x� �   � %               o%   o           %              
"   
 x�           d&    1� �   x� �   � %               o%   o           o%   o           
"   
 x�           �&    1� �   x� �   � %               o%   o           %              
"   
 x�           \'    1� �   x� �   � %               o%   o           o%   o           
"   
 *�           �'    1� �   *� �   � %               o%   o           %              
"   
 *�           T(    1� �   *� �   � %               o%   o           o%   o           
"   
  �           �(    1� �    �   	 � %               o%   o           � P    P �L 
�H T   %              �     }        �GG %              
"   
 y�           �)    1� �   y� �   � %               o%   o           %               
"   
 y�           *    1� �   y� �   � %               o%   o           o%   o           
"   
 ~�           �*    1�    ~� K   � %               o%   o           � P    x
"   
 x�           +    1�    x� K   � %               o%   o           � +  - ~
"   
 �           x+    1� Y   � K   � %               o%   o           � P    x
"   
 *�           �+    1� p   *� K   � %               o%   o           � �   
"   
 � �          `,    1� �   � � �     
"   
 �           �,    1� �   � K   � %               o%   o           � P    
"   
 � �          -    1� �  
 � � �     
"   
 � �          L-    1� �   � � �     
"   
 ~�           �-    1� �   ~�   	 � %               o%   o           � P    x
"   
 x�           �-    1� �   x� K   � %               o%   o           � P    ~
"   
 x�           p.    1� �   x� �   � %               o%   o           o%   o           
"   
 *�           �.    1�    *� K   � %               o%   o           �   ! 
"   
 �           `/    1� <   � K   � %               o%   o           � P    *
"   
  �           �/    1� I    � K   � %               o%   o           � \   
"   
  �           H0    1� k  	  � �   � %               o%   o           o%   o           
"   
 x�           �0    1� u   x� �   � %               o%   o           %               
"   
 � �          @1    1� �   � � �     
"   
 x�           |1    1� �   x� K   � %               o%   o           � �   
"   
 �           �1    1� �   �   	 � %               o%   o           � P    x
"   
 *�           d2    1� �   *�   	 � %               o%   o           � P    
"   
 � �          �2    1� �   � � �     
"   
 � �          3    1� �   � �   	   
"   
  �           P3    1� �    � �   � o%   o           o%   o           %               
"   
 � �          �3    1�    � � �     
"   
 � �          4    1� "   � �   	   
"   
 � �          D4    1� 0   � �   	   
"   
 � �          �4    1� C   � �   	   
"   
 � �          �4    1� T   � �   	   
"   
 � �          �4    1� e   � �   	   
"   
 � �          45    1� v   � � �     
"   
 *�           p5    1� �   *� K   � %               o%   o           � �  4 y
"   
 � �          �5    1� �   � � �     
"   
 � �           6    1� �   � � �     
"   
 � �          \6    1� �   � � �     
"   
 � �          �6    1� �   � �   	   
"   
 � �          �6    1�    � �   	   
"   
 � �          7    1� #   � �   	   
"   
 � �          L7    1� 5   � � �     
"   
 ~�           �7    1� B   ~�   	 � %               o%   o           � P    x
"   
 �           �7    1� P   �   	 � %               o%   o           � P    ~
"   
 y�           p8    1� \   y�   	 � %               o%   o           � P    
"   
 *�           �8    1� q   *�   	 � %               o%   o           � P    y
"   
 �           X9    1� �   � �   � %               o%   o           %               
"   
 �           �9    1� �   � �   � %               o%   o           o%   o           
"   
 �           P:    1� �   � �   � %               o%   o           %               
"   
 x�           �:    1� �   x� �   � %               o%   o           %               
"   
 x�           H;    1� �   x� �   � %               o%   o           o%   o           
"   
 �           �;    1� �   � �   � %               o%   o           %               
"   
 � �          @<    1� �   � �   	   
"   
 �           |<    1� �   � �   � %               o%   o           %              
"   
 � �          �<    1� 
   � �   	   
"   
 � �          4=    1�    � �   	   
"   
 � �          p=    1� %  
 � �   	   
"   
 x�           �=    1� 0   x�   	 � %               o%   o           � �   x
"   
 �            >    1� B   �   	 � %               o%   o           � P    xP �L 
�H T   %              �     }        �GG %              
"   
 �           �>    1� S   � K   � %               o%   o           � P    
"   
 *�           \?    1� a   *� �   � %               o%   o           %               
"   
 �           �?    1� n   � K   � %               o%   o           � P    *
"   
 x�     ,      L@    1� ~   x� K   � %               o%   o           �   � .     � �   ��    	 x
"   
 �           �@    1� �   � �   � %               o%   o           o%   o           
"   
 x�           \A    1� �   x� K   � %               o%   o           � P    ~
"   
 �           �A    1� �   � K   � %               o%   o           � P    x
"   
 �           DB    1� �   �   	 � %               o%   o           o%   o           
"   
 �           �B    1� �   � K   � %               o%   o           o%   o           
"   
  �           <C    1� �    � K   � %               o%   o           � P    
"   
 y�           �C    1� �   y� �   � %               o%   o           %               
"   
 � �          ,D    1� �   � � �     
"   
 ~�           hD    1� 
    ~� K   � %               o%   o           � "   ~ 
"   
 �           �D    1� �    � K   � %               o%   o           � P    ~
"   
 x�           PE    1� �    x� K   � %               o%   o           � �    
"   
 �           �E    1� �    �   	 � %               o%   o           � �    x
"   
 �           8F    1� !   �   	 � %               o%   o           � !   
"   
 *�           �F    1� !  	 *� K   � %               o%   o           �  !   
"   
 y�            G    1� #!  
 y�   	 � %               o%   o           � .!   *
"   
 y�           �G    1� 3!   y� �   � %               o%   o           o%   o           
"   
 ~�           H    1� F!   ~� K   � %               o%   o           � R!   
"   
 �           �H    1� d!   � K   � %               o%   o           � P    ~
"   
 �           �H    1� m!  
 � �   � %               o%   o           o%   o           
"   
 � �          tI    1� x!   � � �     
"   
 �           �I    1� �!   � K   � %               o%   o           � �!  ] 
"   
 x�           $J    1� �!   x� K   � %               o%   o           � P    
"   
 y�           �J    1� "   y� K   � %               o%   o           � "   x
"   
 �           K    1� ""   � �   � %               o%   o           %               
"   
 �           �K    1� �   � K   � %               o%   o           � P    
"   
 �           �K    1� *"   � K   � %               o%   o           o%   o           
"   
 � �          xL    1� <"   � �   	   P �L 
�H T   %              �     }        �GG %              
"   
 �           M    1� M"   � �   � %               o%   o           %               
"   
 �           �M    1� `"  	 � �   � %               o%   o           %               
"   
 � �           N    1� j"   � � K         
�    
�        �     }         �     }        �     }             �     }        �%                  �     }         �     }        �     }             �     }        �%              
�             �G "    � %     start-super-proc q� %     adm2/smart.p �P �L 
�H T   %              �     }        �GG %              
"   
   �       �O    6� �     
"   
   
�        P    8
"   
   �        <P    ��     }        �G 4              
"   
 ߱G %              G %              %� � �   EnabledObjFldsToDisable,ModifyFields,DataSourceNames,UpdateTargetNames,LogicalObjectName,LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout  
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        �Q    �� �   � P   �        �Q    �@    
� @  , 
�       �Q    �� �   �p�               �L
�    %              � 8       R    � $         � �          
�    � �   �
"   
 �p� @  , 
�       S    �� �   �p�               �L"    , �   � �"   � �"   � �     }        �A      |    "      � �"   *%              (<   \ (    |    �     }        �A� �"   �A"  	      "    �"  	    < "    �"  	  (    |    �     }        �A� �"   �A"  	  
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        �T    �� �   � P   �        �T    �@    
� @  , 
�       �T    �� �   �p�               �L
�    %              � 8      U    � $         � �          
�    � �   �
"   
 �p� @  , 
�       V    �� @  
 �p�               �L"    , 
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
 x(�  L ( l       �        �V    �� �   � P   �        �V    �@    
� @  , 
�       �V    �� �   �p�               �L
�    %              � 8      �V    � $         � �   �     
�    � �   � 
"   
 �p� @  , 
�       �W    �� �   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
   
"   
 �
"   
   
"   
   (�  L ( l       �        �X    �� �   � P   �        �X    �@    
� @  , 
�       �X    �� �     p�               �L
�    %              � 8      �X    � $         � �          
�    � �     
"   
 �p� @  , 
�       �Y    �� f  
 �p�               �L%     SmartDataViewer 
"   
   p� @  , 
�       8Z    �� �     p�               �L%      FRAME   
"   
  p� @  , 
�       �Z    �� �    p�               �L%               
"   
  p� @  , 
�       �Z    �� b    p�               �L(        � P      � P      � P      �     }        �A
�H T   %              �     }        �GG %              
"   
 y (   � 
"   
 �    �        �[    �� �   �
"   
   � 8      $\    � $         � �          
�    � �   �
"   
   �        |\    �
"   
   �       �\    /
"   
   
"   
   �       �\    6� �     
"   
   
�        �\    8
"   
   �        ]    �
"   
   �       4]    �
"   
   p�    � �"   *
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 �    �        �]    �A"    �A
"   
   
�        D^    �@ � 
"   
 y"      �       }        �
"   
 � %              %                "    � %     start-super-proc q� %     adm2/appserver.p �*�    � S#     
�    �     }        �%               %      Server  - �     }        �    "  
  � P    � %                   "    � P    � %      NONE    p�,  8         $     "    x        � m#   �
�    
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        �`    �� �   � P   �        �`    �@    
� @  , 
�       �`    �� �   �p�               �L
�    %              � 8      �`    � $         � �          
�    � �   �
"   
 �p� @  , 
�       �a    �� '   �p�               �L"    , p�,  8         $     "  
  x        � {#   �
�     "    � %     start-super-proc p� %     adm2/visual.p �� 
"    
 � %     contextHelp 
"    
   
�    
�    %     processAction   
�    %     CTRL-PAGE-UP �%     processAction   
�    %     CTRL-PAGE-DOWN  "    � %     start-super-proc o� %     adm2/containr.p %     modifyListProperty  
�    
�    %      Add     %      ContainerSourceEvents %      initializeDataObjects 0 0   A    �    � �#   
�    � �#   � A    �    � �#     
�    � $   � %     modifyListProperty  
�    
�    %      Add     %      ContainerSourceEvents �%     buildDataRequest ent0 A    �    � �#   � 
�    �  $   %     modifyListProperty  
�    
�    %      Add     %     SupportedLinks %      ContainerToolbar-Target %               
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
 �(�  L ( l       �        @f    �� �   � P   �        Lf    �@    
� @  , 
�       Xf    �� �   �p�               �L
�    %              � 8      df    � $         � �   �     
�    � �   � 
"   
 �p� @  , 
�       tg    �� �   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
 �(�  L ( l       �         h    �� �   � P   �        ,h    �@    
� @  , 
�       8h    �� �   �p�               �L
�    %              � 8      Dh    � $         � �   �     
�    � �   �
"   
 �p� @  , 
�       Ti    �� �   �p�               �L%               "    � %     start-super-proc o� %     adm2/datavis.p %     modifyListProperty  
�    %      ADD     %     SupportedLinks %     Toolbar-Target %     valueChanged    
�    %     valueChanged    
�     "    � %     start-super-proc n� %     adm2/viewer.p �%     modifyListProperty  
�    
�    %      Add     %     DataSourceEvents ?%     buildDataRequest ?�   � .   � �     � �$  2 ��   � .     � �   �� �$  2 �@    �    � .   �� �$   y     � .   �"    y� .   � �@    �    � .     � �$         � .   y"    � � .     
�H T   %              �     }        �GG %              
"   
 � 
"   
 �
"   
 � 
"   
 � (�  L ( l       �        �l    �� �   � P   �        �l    �@    
� @  , 
�       �l    �� �   � p�               �L
�    %              � 8      �l    � $         � �   �      
�    � �     
"   
 �p� @  , 
�       n    �� n   �p�               �L"    , 
"   
   p� @  , 
�       dn    �� �     p�               �L"    , 
"   
  p� @  , 
�       �n    �� m!  
  p�               �L%               �             I%               �             �%              �     }        �
�                    �           x   `       ��                   /  �                �        O   ����    e�          O   ����    R�          O   ����    ��         $     �   ���                       �[     
                    � ߱                   �          4   �����[                �                      ��                    .                  D7�             (  �  �    0\               �  4          4   �����\                D                      ��                     -                  |@�              �  x  o   !      ,                                 �  �   "  �\      �  �   #  �\      �  $   $  �  ���                        ]     
                    � ߱          �   %   ]         �   &  @]      4  �   )  `]          $   ,  `  ���                       �]  @         |]              � ߱                     (                T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           x   `       ��                 S  �  �               XA�        O   ����    e�          O   ����    R�          O   ����    ��      �"                      �          �  $   e  �   ���                       �]     
                    � ߱                  �  �                      ��                   f  h                  �F�          f  (      4   ����^      $   g  �  ���                       P^     
                    � ߱        d     i    (          4   ����d^      /  j  T                               3   ����x^  x  �   �  �^          O   �  ��  ��  �^               �          �  �   , �                          
                               �      ��                            ����                                                        x   `       ��                  �  �  �               ��r        O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                                            �           x   `       ��                  <  H  �               (@�        O   ����    e�          O   ����    R�          O   ����    ��      �      F  �� �                        G  �   �           4   ����lo      �   G  �o    ��                              ��        �                  ����                                  d d     T   ���  �  � �                                               �      �                                                                 d     D                                                                 P   d �Q                                                           	%  G   
 X d �Q                                                              �  
   g     �       P   � -Q                                                           %  G   
 X � KQ                                                       !      �     g            P   ,vQ                                                            %  G   
 X ,/Q                                                       (        (   g             D                                                                    TXS appSrvUtils RowObject CodCta CodFam DesFam PorDep trg-FchAct trg-HraAct trg-Usuario ASSIGNFOCUSEDWIDGET ASSIGNWIDGETVALUE ASSIGNWIDGETVALUELIST BLANKWIDGET CLEARWIDGET DISABLERADIOBUTTON DISABLEWIDGET ENABLERADIOBUTTON ENABLEWIDGET FORMATTEDWIDGETVALUE FORMATTEDWIDGETVALUELIST HIDEWIDGET HIGHLIGHTWIDGET RESETWIDGETVALUE TOGGLEWIDGET VIEWWIDGET WIDGETHANDLE WIDGETLONGCHARVALUE WIDGETISBLANK WIDGETISFOCUSED WIDGETISMODIFIED WIDGETISTRUE WIDGETVALUE WIDGETVALUELIST F-Main X(10) C�digo de cuenta contable X(4) Codigo de familia X(40) Descripci�n de familia O:\on_in_co\Util\vtablewi.w should only be RUN PERSISTENT. GETTARGETPROCEDURE GETOBJECTTYPE GETSHOWPOPUP SETSHOWPOPUP GETCREATEHANDLES GETDATAMODIFIED GETDISPLAYEDFIELDS GETDISPLAYEDTABLES GETENABLEDFIELDS GETENABLEDHANDLES GETFIELDHANDLES GETFIELDSENABLED GETGROUPASSIGNSOURCE GETGROUPASSIGNSOURCEEVENTS GETGROUPASSIGNTARGET GETGROUPASSIGNTARGETEVENTS GETNEWRECORD GETOBJECTPARENT GETRECORDSTATE GETROWIDENT GETTABLEIOSOURCE GETTABLEIOSOURCEEVENTS GETUPDATETARGET GETUPDATETARGETNAMES GETWINDOWTITLEFIELD OKTOCONTINUE SETCONTAINERMODE SETDATAMODIFIED SETDISPLAYEDFIELDS SETENABLEDFIELDS SETGROUPASSIGNSOURCE SETGROUPASSIGNSOURCEEVENTS SETGROUPASSIGNTARGET SETGROUPASSIGNTARGETEVENTS SETLOGICALOBJECTNAME SETOBJECTPARENT SETSAVESOURCE SETTABLEIOSOURCE SETTABLEIOSOURCEEVENTS SETUPDATETARGET SETUPDATETARGETNAMES SETWINDOWTITLEFIELD SHOWDATAMESSAGES DISABLEPAGESINFOLDER ENABLEPAGESINFOLDER GETCALLERPROCEDURE GETCALLERWINDOW GETCONTAINERMODE GETCONTAINERTARGET GETCONTAINERTARGETEVENTS GETCURRENTPAGE GETDISABLEDADDMODETABS GETDYNAMICSDOPROCEDURE GETFILTERSOURCE GETMULTIINSTANCEACTIVATED GETMULTIINSTANCESUPPORTED GETNAVIGATIONSOURCE GETNAVIGATIONSOURCEEVENTS GETNAVIGATIONTARGET GETOUTMESSAGETARGET GETPAGENTARGET GETPAGESOURCE GETPRIMARYSDOTARGET GETREENABLEDATALINKS GETRUNDOOPTIONS GETRUNMULTIPLE GETSAVEDCONTAINERMODE GETSDOFOREIGNFIELDS GETTOPONLY GETUPDATESOURCE GETWAITFOROBJECT GETWINDOWTITLEVIEWER GETSTATUSAREA PAGENTARGETS SETCALLEROBJECT SETCALLERPROCEDURE SETCALLERWINDOW SETCONTAINERTARGET SETCURRENTPAGE SETDISABLEDADDMODETABS SETDYNAMICSDOPROCEDURE SETFILTERSOURCE SETINMESSAGETARGET SETMULTIINSTANCEACTIVATED SETMULTIINSTANCESUPPORTED SETNAVIGATIONSOURCE SETNAVIGATIONSOURCEEVENTS SETNAVIGATIONTARGET SETOUTMESSAGETARGET SETPAGENTARGET SETPAGESOURCE SETPRIMARYSDOTARGET SETREENABLEDATALINKS SETROUTERTARGET SETRUNDOOPTIONS SETRUNMULTIPLE SETSAVEDCONTAINERMODE SETSDOFOREIGNFIELDS SETTOPONLY SETUPDATESOURCE SETWAITFOROBJECT SETWINDOWTITLEVIEWER SETSTATUSAREA GETALLFIELDHANDLES GETALLFIELDNAMES GETCOL GETDEFAULTLAYOUT GETDISABLEONINIT GETENABLEDOBJFLDS GETENABLEDOBJHDLS GETHEIGHT GETHIDEONINIT GETLAYOUTOPTIONS GETLAYOUTVARIABLE GETOBJECTENABLED GETOBJECTLAYOUT GETROW GETWIDTH GETRESIZEHORIZONTAL GETRESIZEVERTICAL SETALLFIELDHANDLES SETALLFIELDNAMES SETDEFAULTLAYOUT SETDISABLEONINIT SETHIDEONINIT SETLAYOUTOPTIONS SETOBJECTLAYOUT SETRESIZEHORIZONTAL SETRESIZEVERTICAL GETOBJECTTRANSLATED GETOBJECTSECURED CREATEUIEVENTS GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALVERSION SETOBJECTNAME SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE , prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDataViewer ContainerType FRAME PropertyDialog adm2/support/viewerd.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties EnabledObjFldsToDisable,ModifyFields,DataSourceNames,UpdateTargetNames,LogicalObjectName,LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout SupportedLinks Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext ObjectLayout LayoutOptions ObjectEnabled LayoutVariable DefaultLayout DisableOnInit EnabledObjFlds EnabledObjHdls FieldSecurity SecuredTokens AllFieldHandles AllFieldNames MinHeight MinWidth ResizeHorizontal ResizeVertical ObjectSecured ObjectTranslated PopupButtonsInFields ColorInfoBG INTEGER ColorInfoFG ColorWarnBG ColorWarnFG ColorErrorBG ColorErrorFG BGColor FGColor FieldPopupMapping CurrentPage PendingPage ContainerTarget ContainerTargetEvents exitObject,okObject,cancelObject,updateActive ContainerToolbarSource ContainerToolbarSourceEvents toolbar,okObject,cancelObject OutMessageTarget PageNTarget PageSource FilterSource UpdateSource UpdateTarget CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState StartPage RunMultiple WaitForObject DynamicSDOProcedure adm2/dyndata.w RunDOOptions InitialPageList WindowFrameHandle Page0LayoutManager MultiInstanceSupported MultiInstanceActivated ContainerMode SavedContainerMode SdoForeignFields NavigationSource NavigationTarget PrimarySdoTarget NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter CallerWindow CallerProcedure CallerObject DisabledAddModeTabs ReEnableDataLinks WindowTitleViewer UpdateActive InstanceNames ClientNames ContainedDataObjects ContainedAppServices DataContainer HasDbAwareObjects HasDynamicProxy HideOnClose HideChildContainersOnClose HasObjectMenu RequiredPages RemoveMenuOnHide ProcessList PageLayoutInfo PageTokens DataContainerName WidgetIDFileName CreateHandles DataModified DisplayedFields DisplayedTables   Editable EnabledFields EnabledHandles EnabledObjFldsToDisable EnabledWhenNew FieldHandles FieldsEnabled GroupAssignSource GroupAssignSourceEvents addRecord,copyRecord,updateRecord,resetRecord,undoRecord,cancelRecord,enableFields,disableFields,collectChanges,validateFields GroupAssignTarget GroupAssignTargetEvents updateState,LinkState InternalDisplayFromSource (Large) ModifyFields (All) NewRecord No ObjectMode View PrintPreviewActive RecordState NoRecordAvailable RowIdent SaveSource TableIOSource TableIOSourceEvents addRecord,updateRecord,copyRecord,deleteRecord,resetRecord,undoChange,cancelRecord,updateMode ToolbarSource ToolbarSourceEvents toolbar UndoNew UpdateTargetNames WindowTitleField KeepChildPositions ShowPopup FieldWidgetIDs ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService cFields adm2/visual.p CTRL-PAGE-UP CTRL-PAGE-DOWN adm2/containr.p Add initializeDataObjects getSupportedLinks data-target data-source buildDataRequest containertoolbar-target ContainerToolbar-Target adm2/datavis.p ADD Toolbar-Target DISPLAYOBJECTS cViewCols cEnabled iCol iEntries cEntry adm2/viewer.p RowObject.CodCta RowObject.CodFam RowObject.DesFam ,RowObject. DISABLE_UI default Cuenta contable Codigo Descripci�n �  l"  �  +      3 �    ��      0         pcFieldType     ��      T         pcColValues     ��      x         pcValue     ��      �         pcState �   ��      �         pcChanges       ��      �         pcChanges       ��               plCancel        ��      $        plAnswer        ��      H        plCancel        ��      l        pcRelative  �  ��      �        pcAction        ��      �        pcAction        ��      �        pcState     ��      �        pcReturn        ��              pcMode      ��      <        pcState     ��      \        pcNotValidFields    �  ��      �        pcProp      ��      �        pcProp      ��      �        plCancel    �  ��      �        pcProcName    ��             
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
 hTarget �  ��      �        pcMessage       ��      �        pcMessage       ��      �        plEnabled             	     cType       T	     T   �          D	                  getObjectType     3  5  �	        t	  
   hReposBuffer    �	        �	  
   hPropTable  �	        �	  
   hBuffer           �	  
   hTable  	  
     U   `	          
                  adm-clone-props              !  "  #  $  %  &  )  ,  -  .  /            t
  
   hProc             �
        pcProcName  �	  �
  	   V   `
  |
      �
                  start-super-proc    e  f  g  h  i  j  �  �  �  �
  8     W                                   >    l     X                                   B  C  <  �     Y                                   F  G  t  �     Z               �                  displayObjects  �  �        [                                   �  �  T     \                                     $  �     ]               �                  disable_UI  F  G  H  X  h  $    
 �      L                          �  �     RowObject   @         H         P         X         `         l         x         CodCta  CodFam  DesFam  PorDep  trg-FchAct  trg-HraAct  trg-Usuario �          �  
   appSrvUtils �        �  
   gshAstraAppserver   �        �  
   gshSessionManager             
   gshRIManager    @        ,  
   gshSecurityManager  h        T  
   gshProfileManager   �        |  
   gshRepositoryManager    �  	 	     �  
   gshTranslationManager   �  
 
     �  
   gshWebManager           �     gscSessionId    ,             gsdSessionObj   P        @  
   gshFinManager   t        d  
   gshGenManager   �        �  
   gshAgnManager   �        �     gsdTempUniqueID �        �     gsdUserObj          �     gsdRenderTypeObj    ,             gsdSessionScopeObj  H       @  
   ghProp  h       \  
   ghADMProps  �       |  
   ghADMPropsBuf   �       �     glADMLoadFromRepos  �       �     glADMOk �       �  
   ghContainer             cObjectName ,    	   $     iStart  L    
   @     cAppService l       `     cASDivision �       �     cServerOperatingMode    �       �     cFields �       �     cViewCols   �       �     cEnabled                iCol    0       $     iEntries             D     cEntry        X  \  RowObject            H   E  F  H  I  �	  �	  �	  �	  
  
  
  
  
  
  
  
  
  
  
   
  !
  "
  $
  &
  (
  *
  +
  ,
  /
  1
  2
  4
  5
  6
  7
  8
  >
  @
  F
  H
  J
  K
  Q
  R
  S
  T
  W
  X
  Z
  [
  ]
  ^
  _
  `
  a
  b
  c
  e
  f
  g
  i
  j
  k
  l
  m
  �
  U  V  Y  Z  [  \  ]  ^  _  `  a  b  c  d  e  f  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �               v  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  4  B  C  E  F  G  H  N  O  P  Q  R  S  T  U  V  X  Y  Z  [  \  ]  ^  _  `  a  b  c  e  f  h  i  j  k  �  �  �  �  �  �  `  "  #  %  &  )  *  ,  4  m  n  w  x  |  }  ~  �  �  �  �  �  �  �  �    �  �  �  �  �  �  �  �  �  �  �  �  
    Q  �  �  ;  <  =  ?  A  E  ^  _  `  b  j  p  v  y  ~  �  �    P  Q  R  T  V  �                "        :%  C:\Progress\OpenEdge\src\adm2\viewer.i   �  �Q 2 %C:\Progress\OpenEdge\src\adm2\custom\viewercustom.i  ,  } & C:\Progress\OpenEdge\src\adm2\datavis.i  l  � 1 %C:\Progress\OpenEdge\src\adm2\custom\dataviscustom.i �  f! ' C:\Progress\OpenEdge\src\adm2\containr.i �  � 0 %C:\Progress\OpenEdge\src\adm2\custom\containrcustom.i      �� ( C:\Progress\OpenEdge\src\adm2\visual.i   X  # / %C:\Progress\OpenEdge\src\adm2\custom\visualcustom.i  �  �< ) C:\Progress\OpenEdge\src\adm2\appserver.i    �  �� . %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i     I� * C:\Progress\OpenEdge\src\adm2\smart.i    H  Ds - C:\Progress\OpenEdge\gui\fn  |  tw , %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   �  Q. + C:\Progress\OpenEdge\gui\set �  �/  C:\Progress\OpenEdge\src\adm2\viewprop.i   �� $ %C:\Progress\OpenEdge\src\adm2\custom\viewpropcustom.i    @  ۃ % %C:\Progress\OpenEdge\src\adm2\custom\viewprtocustom.i    �  ��  C:\Progress\OpenEdge\src\adm2\dvisprop.i �  B� " %C:\Progress\OpenEdge\src\adm2\custom\dvispropcustom.i    �  �� # %C:\Progress\OpenEdge\src\adm2\custom\dvisprtocustom.i    @  ��  C:\Progress\OpenEdge\src\adm2\cntnprop.i �  ��   %C:\Progress\OpenEdge\src\adm2\custom\cntnpropcustom.i    �  P ! %C:\Progress\OpenEdge\src\adm2\custom\cntnprtocustom.i    �  F>  C:\Progress\OpenEdge\src\adm2\visprop.i  @  �I  %C:\Progress\OpenEdge\src\adm2\custom\vispropcustom.i t  ��  %C:\Progress\OpenEdge\src\adm2\custom\visprtocustom.i �  �l  C:\Progress\OpenEdge\src\adm2\appsprop.i �  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    (  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    l  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i �  �j  C:\Progress\OpenEdge\gui\get �  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i      ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    P  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i �  Su  C:\Progress\OpenEdge\src\adm2\globals.i  �  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i �  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   <  �  C:\Progress\OpenEdge\src\adm2\appsprto.i �  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   �  �X  C:\Progress\OpenEdge\src\adm2\visprto.i  �  !�  %C:\Progress\OpenEdge\src\adm2\custom\visualdefscustom.i  0   n�  C:\Progress\OpenEdge\src\adm2\cntnprto.i t   ;  %C:\Progress\OpenEdge\src\adm2\custom\containrdefscustom.i    �   �7 
 C:\Progress\OpenEdge\src\adm2\dvisprto.i �   0 	 %C:\Progress\OpenEdge\src\adm2\custom\datavisdefscustom.i $!  ��  C:\Progress\OpenEdge\src\adm2\viewprto.i h!  gf  %C:\Progress\OpenEdge\src\adm2\custom\viewerdefscustom.i  �!  ~�  C:\Progress\OpenEdge\src\adm2\widgetprto.i   �!  N^  .\util\dtables.i "  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   4"  I�    O:\on_in_co\Util\vtablewi.w      �         �"  �   �     �"     �  2   �"  �   �     �"     b  +   �"  �   _     �"     =  +   �"  �   <     #       +   #  \   �     $#  o   �  &   4#     Z  1   D#  U   @  &   T#  �   9  '   d#       +   t#  �     '   �#     �  +   �#  �   �  '   �#     �  0   �#  �   y  '   �#     w  -   �#  �   p  '   �#     n  -   �#  �   m  '   $     k  -   $  r   O  '   $$  n   7  (   4$     �  /   D$  P   �  (   T$  �   �  )   d$     `  .   t$  �   [  )   �$     9  +   �$  �   8  )   �$       +   �$  �     )   �$     �  +   �$  g   �  )   �$     �     �$  O   �  )   %  �   +  *   %     )  -   $%  �   �  *   4%     �  ,   D%  �   �  *   T%     t  +   d%  �   s  *   t%     Q  +   �%  �   P  *   �%     .  +   �%  �   -  *   �%       +   �%  �   �  *   �%     �  +   �%  �   �  *   �%     �  +   &  }   �  *   &     �  +   $&     	  *   4&     �  )   D&     l  (   T&     �  '   d&     �  &   t&     i     �&  u   `     �&  O   R  $   �&     A  %   �&     �  $   �&  h   �     �&  �   �     �&  O   �  "   �&     �  #   '     p  "   '  {   =     $'  �   4     4'  O   &      D'       !   T'     �      d'  �        t'  �   v     �'  O   h     �'     W     �'     	     �'  �   �     �'  x   �     �'  M   �     �'     �     �'     j     (  a   S     (  �  2     $(          4(  �  �
     D(  O   �
     T(     �
     d(     s
     t(  �   �	     �(     o     �(     �     �(  x   �     �(     �     �(     .     �(     *     �(          �(     �     )  Q   �     )     �     $)     [     4)     G     D)     -     T)  f        d)     �     t)  "   ]     �)     I     �)     (     �)  Z   �     �)     �     �)     �     �)     �     �)     r     �)  X   O     *     �  
   *      a     $*     M  	   4*     .     D*  ]   #     T*     �     d*     �     t*     �     �*     y     �*     \     �*  0   �       �*     Q      �*     .       �*     &      �*     !       �*           