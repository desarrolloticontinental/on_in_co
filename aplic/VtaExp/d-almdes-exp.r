	��Vg_}a�4    �                                              � 34B4010Autf-8 MAIN d:\newsie\on_in_co\APLIC\vtaexp\d-almdes-exp.w,, PROCEDURE enable_UI,, PROCEDURE disable_UI,, PROCEDURE adm-create-objects,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE startServerObject,, PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE restartServerObject,, PROCEDURE initializeServerObject,, PROCEDURE disconnectObject,, PROCEDURE destroyServerObject,, PROCEDURE bindServer,, PROCEDURE processAction,,INPUT pcAction CHARACTER PROCEDURE enableObject,, PROCEDURE disableObject,, PROCEDURE applyLayout,, PROCEDURE viewPage,,INPUT piPageNum INTEGER PROCEDURE viewObject,, PROCEDURE toolbar,,INPUT pcValue CHARACTER PROCEDURE selectPage,,INPUT piPageNum INTEGER PROCEDURE removePageNTarget,,INPUT phTarget HANDLE,INPUT piPage INTEGER PROCEDURE passThrough,,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER PROCEDURE notifyPage,,INPUT pcProc CHARACTER PROCEDURE initPages,,INPUT pcPageList CHARACTER PROCEDURE initializeVisualContainer,, PROCEDURE initializeObject,, PROCEDURE hidePage,,INPUT piPageNum INTEGER PROCEDURE destroyObject,, PROCEDURE deletePage,,INPUT piPageNum INTEGER PROCEDURE createObjects,, PROCEDURE constructObject,,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE PROCEDURE confirmExit,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE changePage,, PROCEDURE assignPageProperty,,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION createUiEvents,LOGICAL, FUNCTION getObjectSecured,LOGICAL, FUNCTION getObjectTranslated,LOGICAL, FUNCTION setResizeVertical,LOGICAL,INPUT plResizeVertical LOGICAL FUNCTION setResizeHorizontal,LOGICAL,INPUT plResizeHorizontal LOGICAL FUNCTION setObjectLayout,LOGICAL,INPUT pcLayout CHARACTER FUNCTION setLayoutOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setHideOnInit,LOGICAL,INPUT plHide LOGICAL FUNCTION setDisableOnInit,LOGICAL,INPUT plDisable LOGICAL FUNCTION setDefaultLayout,LOGICAL,INPUT pcDefault CHARACTER FUNCTION setAllFieldNames,LOGICAL,INPUT pcValue CHARACTER FUNCTION setAllFieldHandles,LOGICAL,INPUT pcValue CHARACTER FUNCTION getResizeVertical,LOGICAL, FUNCTION getResizeHorizontal,LOGICAL, FUNCTION getWidth,DECIMAL, FUNCTION getRow,DECIMAL, FUNCTION getObjectLayout,CHARACTER, FUNCTION getObjectEnabled,LOGICAL, FUNCTION getLayoutVariable,CHARACTER, FUNCTION getLayoutOptions,CHARACTER, FUNCTION getHideOnInit,LOGICAL, FUNCTION getHeight,DECIMAL, FUNCTION getEnabledObjHdls,CHARACTER, FUNCTION getEnabledObjFlds,CHARACTER, FUNCTION getDisableOnInit,LOGICAL, FUNCTION getDefaultLayout,CHARACTER, FUNCTION getCol,DECIMAL, FUNCTION getAllFieldNames,CHARACTER, FUNCTION getAllFieldHandles,CHARACTER, FUNCTION setStatusArea,LOGICAL,INPUT plStatusArea LOGICAL FUNCTION getObjectType,character, FUNCTION setWindowTitleViewer,LOGICAL,INPUT phViewer HANDLE FUNCTION setWaitForObject,LOGICAL,INPUT phObject HANDLE FUNCTION setUpdateTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setUpdateSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setTopOnly,LOGICAL,INPUT plTopOnly LOGICAL FUNCTION setSdoForeignFields,LOGICAL,INPUT cSdoForeignFields CHARACTER FUNCTION setSavedContainerMode,LOGICAL,INPUT cSavedContainerMode CHARACTER FUNCTION setRunMultiple,LOGICAL,INPUT plMultiple LOGICAL FUNCTION setRunDOOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setRouterTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setReEnableDataLinks,LOGICAL,INPUT cReEnableDataLinks CHARACTER FUNCTION setPrimarySdoTarget,LOGICAL,INPUT hPrimarySdoTarget HANDLE FUNCTION setPageSource,LOGICAL,INPUT phObject HANDLE FUNCTION setPageNTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setOutMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setNavigationTarget,LOGICAL,INPUT cTarget CHARACTER FUNCTION setNavigationSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setNavigationSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setMultiInstanceSupported,LOGICAL,INPUT lMultiInstanceSupported LOGICAL FUNCTION setMultiInstanceActivated,LOGICAL,INPUT lMultiInstanceActivated LOGICAL FUNCTION setInMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setFilterSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDynamicSDOProcedure,LOGICAL,INPUT pcProc CHARACTER FUNCTION setDisabledAddModeTabs,LOGICAL,INPUT cDisabledAddModeTabs CHARACTER FUNCTION setCurrentPage,LOGICAL,INPUT iPage INTEGER FUNCTION setContainerTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setContainerMode,LOGICAL,INPUT cContainerMode CHARACTER FUNCTION setCallerWindow,LOGICAL,INPUT h HANDLE FUNCTION setCallerProcedure,LOGICAL,INPUT h HANDLE FUNCTION setCallerObject,LOGICAL,INPUT h HANDLE FUNCTION pageNTargets,CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER FUNCTION getStatusArea,LOGICAL, FUNCTION getWindowTitleViewer,HANDLE, FUNCTION getWaitForObject,HANDLE, FUNCTION getUpdateTarget,CHARACTER, FUNCTION getUpdateSource,CHARACTER, FUNCTION getTopOnly,LOGICAL, FUNCTION getSdoForeignFields,CHARACTER, FUNCTION getSavedContainerMode,CHARACTER, FUNCTION getRunMultiple,LOGICAL, FUNCTION getRunDOOptions,CHARACTER, FUNCTION getReEnableDataLinks,CHARACTER, FUNCTION getPrimarySdoTarget,HANDLE, FUNCTION getPageSource,HANDLE, FUNCTION getPageNTarget,CHARACTER, FUNCTION getOutMessageTarget,HANDLE, FUNCTION getNavigationTarget,HANDLE, FUNCTION getNavigationSourceEvents,CHARACTER, FUNCTION getNavigationSource,CHARACTER, FUNCTION getMultiInstanceSupported,LOGICAL, FUNCTION getMultiInstanceActivated,LOGICAL, FUNCTION getFilterSource,HANDLE, FUNCTION getDynamicSDOProcedure,CHARACTER, FUNCTION getDisabledAddModeTabs,CHARACTER, FUNCTION getCurrentPage,INTEGER, FUNCTION getContainerTargetEvents,CHARACTER, FUNCTION getContainerTarget,CHARACTER, FUNCTION getContainerMode,CHARACTER, FUNCTION getCallerWindow,HANDLE, FUNCTION getCallerProcedure,HANDLE, FUNCTION enablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION disablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION widgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION widgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION widgetIsTrue,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsModified,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetIsFocused,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsBlank,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetLongcharValue,LONGCHAR,INPUT pcName CHARACTER FUNCTION widgetHandle,HANDLE,INPUT pcName CHARACTER FUNCTION viewWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION toggleWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION resetWidgetValue,LOGICAL,INPUT pcNameList CHARACTER FUNCTION highlightWidget,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER FUNCTION hideWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION formattedWidgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION formattedWidgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION enableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION enableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION disableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION disableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION clearWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION blankWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION assignWidgetValueList,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION assignWidgetValue,LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER FUNCTION assignFocusedWidget,LOGICAL,INPUT pcName CHARACTER     0              0�              Ui 0   �              �]              �#    +   �8 �  7   �= `  8   �@ �   =   �A 8  >    C L  ?           lF �  <H �  ? 4J !  iSO8859-1                                                                           H    �                                      �                  p�                �  p    �   �   Ԁ  �         ��  �   �                                                               PROGRESS                                    
    
                    �              �                                                                                                     
  4         �          X  l  M   �     ֹ  C(�\�                                 �      �   �  0
      ,  
    
                    �             �                                                                                          0
          
  \  B
      �  
    
                  �  �             H                                                                                          B
          
    T
      �  
    
                  p  8             �                                                                                          T
          
  �  a
      0  
    
                    �             �                                                                                          a
          
  `  t
      �  
    
                  �  �             L                                                                                          t
          
    �
      �  
    
                  t  <             �                                                                                          �
          
  �  �
      4  
    
                     �  	           �                                                                                          �
          
  d  �
      �  
    
                  �  �  
           P                                                                                          �
          
    �
      �                         x  @             �                                                                                          �
            �  �
      8                        $  �             �                                                                                          �
            h	  �
      �  
    
                  �  �	             T	                                                                                          �
          
  
  �
      �	  
    
                  |	  D
              
                                                                                          �
          
  �
  �
      <
  
    
                  (
  �
             �
                                                                                          �
          
  l        �
                        �
  �             X                                                                                                              �                        �  H                                                                                                                   �        @                        ,  �             �                                                                                                          0      �                        �                 \                                                                                          0                          ̴                                               д          �  �  8 �            
             
             
                                         
                                                                                                                8   H   X   h   x   �   �   �   �   �   �   �   �       8   H   X   h   x   �   �   �   �   �   �   �   �                                                                                                                                     	                  
                                                                                                                                                                                
                                                     
                                    �  �  �  �  �          �                   $            (             <  H  P  h  \          l              �  �  �  �  �                          �  �  �  �  �                          �                                    $  ,  4  d  L                          h  p  x  �  �                          �  �  �  �  �                          �  �  �                               $  ,  4  \  H                          `  h  p  �  �                          �  �  �  �  �          �              �      <  $                          @  H  P  `  X                          d  l  t  �  |                          �  �  �  �  �          �              �  �  �  �                              �  �  �  �                              �                                     $  0  8  D                              H  T  \  h                                                                          CodCia  999 Cia Cia 0   C�digo de Compa�ia  CodAlm  x(5)    Almac�n Almac�n     C�digo de almac�n   Descripcion X(40)   Descripci�n Descripci�n     Descripci�n de almac�n  TdoArt  Si/No   Todos los Articulos Todos los!Articulos Si  AutMov  Si/No   Autorizacion de Movimientos Aut!Mov Si  DirAlm  X(60)   Direccion   Direccion       HorRec  X(40)   Horario de Recepcion    Horario de Recepcion        EncAlm  X(30)   Encargado del Almacen   Encargado del Almacen       TelAlm  X(13)   Telefono Almacen    Telefono!Almacen        CorrSal 999999  Correlativo Salida  Correlativo!Salida  0   CorrIng 999999  Correlativo Ingreso Correlativo Ingreso 0   CorrTrf 999999  Correlativo Transferencia   Correlativo Transferencia   0   CodDiv  XX-XXX  Divisionaria    Divisionaria    00000   Codigo de Divisionaria  Clave   X(8)    Clave de Modificacion   Clave de!Modificacion       AlmCsg  yes/no  AlmCsg  AlmCsg  no  TpoCsg  X(1)    TpoCsg  TpoCsg      CodCli  x(11)   CodCli  CodCli      Codigo del Cliente  flgrep  yes/no  flgrep  no  Campo-C X(10)   Campo-C     AlmPrincipal    yes/no  AlmPrincipal    no  AlmDespacho yes/no  AlmDespacho no  Campo-Log   yes/no  Campo-Log   no  �  &�  ���������          00000     �           �  �           �                       �     i  i     	 	       $      0   7   >   E   L   S   Z   b   j   r   y      �   �   �   �   �   �   �     ��                                                                              `          ����                            L    �  2                 ��             undefined                                                               �       �  �   l   ��                        �����                Ӱ                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D       �
     C          assignFocusedWidget         �      �     �       LOGICAL,INPUT pcName CHARACTER  assignWidgetValue   �      �      $    �       LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER  assignWidgetValueList         \      �    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER  blankWidget t      �                LOGICAL,INPUT pcNameList CHARACTER  clearWidget �      @      l          LOGICAL,INPUT pcNameList CHARACTER  disableRadioButton  L      �      �          LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    disableWidget   �            4    -      LOGICAL,INPUT pcNameList CHARACTER  enableRadioButton         X      �    ;      LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    enableWidget    l      �      �    M      LOGICAL,INPUT pcNameList CHARACTER  formattedWidgetValue    �             X  	  Z      CHARACTER,INPUT pcName CHARACTER    formattedWidgetValueList    8      |      �  
  o      CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    hideWidget  �      �      (   
 �      LOGICAL,INPUT pcNameList CHARACTER  highlightWidget       L      |    �      LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER  resetWidgetValue    \      �      �    �      LOGICAL,INPUT pcNameList CHARACTER  toggleWidget    �            H    �      LOGICAL,INPUT pcNameList CHARACTER  viewWidget  (      l      �   
 �      LOGICAL,INPUT pcNameList CHARACTER  widgetHandle    x      �      �    �      HANDLE,INPUT pcName CHARACTER   widgetLongcharValue �            @    �      LONGCHAR,INPUT pcName CHARACTER widgetIsBlank          `      �    �      LOGICAL,INPUT pcNameList CHARACTER  widgetIsFocused p      �      �    �      LOGICAL,INPUT pcName CHARACTER  widgetIsModified    �      	      8	          LOGICAL,INPUT pcNameList CHARACTER  widgetIsTrue    	      \	      �	          LOGICAL,INPUT pcName CHARACTER  widgetValue l	      �	      �	    )      CHARACTER,INPUT pcName CHARACTER    widgetValueList �	      �	      ,
    5      CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER        u   ����  �             d   �           p   �              � ߱            Z   �����
   �p
                     �    �    �      |       4   ����|                 �                      ��                  �  �                  �x�                       �        �  �  �      �       4   �����       $  �  �  ���                       �   @         �               � ߱              �  0  @            4   ����      $  �  l  ���                       L  @         8              � ߱        assignPageProperty                              0        ��                      H              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             `               ��                  �           ��                            ����                            changePage                              �  h      ��                      �              (P�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmExit                             �  h      ��                      �              Q�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            constructObject                             �  �      ��                      �              tʱ                    O   ����    e�          O   ����    R�          O   ����    ��            ��                �               �� 
  8               
             ��   `             ,               �� 
                 T  
         ��                            ����                            createObjects                               P  8      ��                      h              �0�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deletePage                              P  8      ��                       h              43�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            destroyObject                               |  d      ��                  "  #  �              �3�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hidePage                                |  d      ��                  %  '  �              �$�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            initializeObject                                �  �      ��                  )  *  �               İ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeVisualContainer                               �  �      ��                  ,  -  �              �İ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initPages                               �  �      ��                  /  1  �              \Ű                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            notifyPage                              �  �      ��                  3  5  �              `��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                             ��                            ����                            passThrough                               �      ��                  7  :  $              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   p             <               ��                  d           ��                            ����                            removePageNTarget                               d  L      ��                  <  ?  |              ć�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �             �  
             ��                  �           ��                            ����                            selectPage                              �  �      ��                  A  C  �              ̇�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            toolbar                             �  �      ��                  E  G  �              `!�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                             ��                            ����                            viewObject                                  �      ��                  I  J                 t��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewPage                                 !  �       ��                  L  N  !              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  0!           ��                            ����                            disablePagesInFolder    
      �!      �!    �      LOGICAL,INPUT pcPageInformation CHARACTER   enablePagesInFolder �!      �!      0"    �      LOGICAL,INPUT pcPageInformation CHARACTER   getCallerProcedure  "      \"      �"    �      HANDLE, getCallerWindow p"      �"      �"    �      HANDLE, getContainerMode    �"      �"      #    �      CHARACTER,  getContainerTarget  �"      #      D#    �      CHARACTER,  getContainerTargetEvents    $#      P#      �#    �      CHARACTER,  getCurrentPage  l#      �#      �#          INTEGER,    getDisabledAddModeTabs  �#      �#      $     "      CHARACTER,  getDynamicSDOProcedure  �#      $      P$  !  9      CHARACTER,  getFilterSource 0$      \$      �$  "  P      HANDLE, getMultiInstanceActivated   l$      �$      �$  #  `      LOGICAL,    getMultiInstanceSupported   �$      �$      %  $  z      LOGICAL,    getNavigationSource �$      $%      X%  %  �      CHARACTER,  getNavigationSourceEvents   8%      d%      �%  &  �      CHARACTER,  getNavigationTarget �%      �%      �%  '  �      HANDLE, getOutMessageTarget �%      �%      &  (  �      HANDLE, getPageNTarget  �%      $&      T&  )  �      CHARACTER,  getPageSource   4&      `&      �&  *  �      HANDLE, getPrimarySdoTarget p&      �&      �&  +        HANDLE, getReEnableDataLinks    �&      �&      '  ,        CHARACTER,  getRunDOOptions �&      '      H'  -  0      CHARACTER,  getRunMultiple  ('      T'      �'  .  @      LOGICAL,    getSavedContainerMode   d'      �'      �'  /  O      CHARACTER,  getSdoForeignFields �'      �'      (  0  e      CHARACTER,  getTopOnly  �'      (      @(  1 
 y      LOGICAL,    getUpdateSource  (      L(      |(  2  �      CHARACTER,  getUpdateTarget \(      �(      �(  3  �      CHARACTER,  getWaitForObject    �(      �(      �(  4  �      HANDLE, getWindowTitleViewer    �(       )      8)  5  �      HANDLE, getStatusArea   )      @)      p)  6  �      LOGICAL,    pageNTargets    P)      |)      �)  7  �      CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER setCallerObject �)      �)      *  8  �      LOGICAL,INPUT h HANDLE  setCallerProcedure  �)      ,*      `*  9  �      LOGICAL,INPUT h HANDLE  setCallerWindow @*      x*      �*  :        LOGICAL,INPUT h HANDLE  setContainerMode    �*      �*      �*  ;        LOGICAL,INPUT cContainerMode CHARACTER  setContainerTarget  �*      +      P+  <  )      LOGICAL,INPUT pcObject CHARACTER    setCurrentPage  0+      t+      �+  =  <      LOGICAL,INPUT iPage INTEGER setDisabledAddModeTabs  �+      �+      �+  >  K      LOGICAL,INPUT cDisabledAddModeTabs CHARACTER    setDynamicSDOProcedure  �+      (,      `,  ?  b      LOGICAL,INPUT pcProc CHARACTER  setFilterSource @,      �,      �,  @  y      LOGICAL,INPUT phObject HANDLE   setInMessageTarget  �,      �,      -  A  �      LOGICAL,INPUT phObject HANDLE   setMultiInstanceActivated   �,      $-      `-  B  �      LOGICAL,INPUT lMultiInstanceActivated LOGICAL   setMultiInstanceSupported   @-      �-      �-  C  �      LOGICAL,INPUT lMultiInstanceSupported LOGICAL   setNavigationSource �-      �-      0.  D  �      LOGICAL,INPUT pcSource CHARACTER    setNavigationSourceEvents   .      T.      �.  E  �      LOGICAL,INPUT pcEvents CHARACTER    setNavigationTarget p.      �.      �.  F  �      LOGICAL,INPUT cTarget CHARACTER setOutMessageTarget �.      /      </  G        LOGICAL,INPUT phObject HANDLE   setPageNTarget  /      \/      �/  H  &      LOGICAL,INPUT pcObject CHARACTER    setPageSource   l/      �/      �/  I  5      LOGICAL,INPUT phObject HANDLE   setPrimarySdoTarget �/       0      40  J  C      LOGICAL,INPUT hPrimarySdoTarget HANDLE  setReEnableDataLinks    0      \0      �0  K  W      LOGICAL,INPUT cReEnableDataLinks CHARACTER  setRouterTarget t0      �0      �0  L  l      LOGICAL,INPUT phObject HANDLE   setRunDOOptions �0      1      @1  M  |      LOGICAL,INPUT pcOptions CHARACTER   setRunMultiple   1      d1      �1  N  �      LOGICAL,INPUT plMultiple LOGICAL    setSavedContainerMode   t1      �1      �1  O  �      LOGICAL,INPUT cSavedContainerMode CHARACTER setSdoForeignFields �1      2      P2  P  �      LOGICAL,INPUT cSdoForeignFields CHARACTER   setTopOnly  02      |2      �2  Q 
 �      LOGICAL,INPUT plTopOnly LOGICAL setUpdateSource �2      �2      �2  R  �      LOGICAL,INPUT pcSource CHARACTER    setUpdateTarget �2      3      L3  S  �      LOGICAL,INPUT pcTarget CHARACTER    setWaitForObject    ,3      p3      �3  T  �      LOGICAL,INPUT phObject HANDLE   setWindowTitleViewer    �3      �3      �3  U        LOGICAL,INPUT phViewer HANDLE   getObjectType   �3      4      L4  V        CHARACTER,  setStatusArea   ,4      X4      �4  W  $      LOGICAL,INPUT plStatusArea LOGICAL  applyLayout                             <5  $5      ��                  �  �  T5              @`�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableObject                               @6  (6      ��                  �  �  X6              �`�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            enableObject                                D7  ,7      ��                  �  �  \7              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                L8  48      ��                  �  �  d8              h��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            processAction                               P9  89      ��                  �  �  h9              h��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �9           ��                            ����                            getAllFieldHandles  h4      �9      :  X  2      CHARACTER,  getAllFieldNames    �9      (:      \:  Y  E      CHARACTER,  getCol  <:      h:      �:  Z  V      DECIMAL,    getDefaultLayout    p:      �:      �:  [  ]      CHARACTER,  getDisableOnInit    �:      �:      ;  \  n      LOGICAL,    getEnabledObjFlds   �:      ;      P;  ]        CHARACTER,  getEnabledObjHdls   0;      \;      �;  ^  �      CHARACTER,  getHeight   p;      �;      �;  _ 	 �      DECIMAL,    getHideOnInit   �;      �;      <  `  �      LOGICAL,    getLayoutOptions    �;      <      D<  a  �      CHARACTER,  getLayoutVariable   $<      P<      �<  b  �      CHARACTER,  getObjectEnabled    d<      �<      �<  c  �      LOGICAL,    getObjectLayout �<      �<       =  d  �      CHARACTER,  getRow  �<      =      4=  e  �      DECIMAL,    getWidth    =      @=      l=  f        DECIMAL,    getResizeHorizontal L=      x=      �=  g        LOGICAL,    getResizeVertical   �=      �=      �=  h  #      LOGICAL,    setAllFieldHandles  �=      �=      ,>  i  5      LOGICAL,INPUT pcValue CHARACTER setAllFieldNames    >      L>      �>  j  H      LOGICAL,INPUT pcValue CHARACTER setDefaultLayout    `>      �>      �>  k  Y      LOGICAL,INPUT pcDefault CHARACTER   setDisableOnInit    �>      �>      ,?  l  j      LOGICAL,INPUT plDisable LOGICAL setHideOnInit   ?      L?      |?  m  {      LOGICAL,INPUT plHide LOGICAL    setLayoutOptions    \?      �?      �?  n  �      LOGICAL,INPUT pcOptions CHARACTER   setObjectLayout �?      �?      $@  o  �      LOGICAL,INPUT pcLayout CHARACTER    setResizeHorizontal @      H@      |@  p  �      LOGICAL,INPUT plResizeHorizontal LOGICAL    setResizeVertical   \@      �@      �@  q  �      LOGICAL,INPUT plResizeVertical LOGICAL  getObjectTranslated �@      A      8A  r  �      LOGICAL,    getObjectSecured    A      DA      xA  s  �      LOGICAL,    createUiEvents  XA      �A      �A  t  �      LOGICAL,    bindServer                              PB  8B      ��                  �  �  hB              dj�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               TC  <C      ��                  �  �  lC              k�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             \D  DD      ��                  �  �  tD              `ҳ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                dE  LE      ��                  �  �  |E              ӳ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              pF  XF      ��                  �  �  �F              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             xG  `G      ��                  �  �  �G              $��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             |H  dH      ��                  �  �  �H              ̊�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �H  
         ��                            ����                            startServerObject                               �I  �I      ��                  �  �  �I              0c�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                �J  �J      ��                  �  �  �J              �c�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �J           ��                            ����                            getAppService   �A      HK      xK  u  	      CHARACTER,  getASBound  XK      �K      �K  v 
 	      LOGICAL,    getAsDivision   �K      �K      �K  w  	      CHARACTER,  getASHandle �K      �K      $L  x  +	      HANDLE, getASHasStarted L      ,L      \L  y  7	      LOGICAL,    getASInfo   <L      hL      �L  z 	 G	      CHARACTER,  getASInitializeOnRun    tL      �L      �L  {  Q	      LOGICAL,    getASUsePrompt  �L      �L      M  |  f	      LOGICAL,    getServerFileName   �L       M      TM  }  u	      CHARACTER,  getServerOperatingMode  4M      `M      �M  ~  �	      CHARACTER,  runServerProcedure  xM      �M      �M    �	      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   �M      N      LN  �  �	      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   ,N      tN      �N  �  �	      LOGICAL,INPUT pcDivision CHARACTER  setASHandle �N      �N      �N  �  �	      LOGICAL,INPUT phASHandle HANDLE setASInfo   �N      O      @O  � 	 �	      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun     O      `O      �O  �  �	      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  xO      �O      �O  �  �	      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   �O      P      @P  �  
      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode   P      dP      �P  �  
      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             XQ  @Q      ��                  �  �  pQ              ��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �Q             �Q  
             ��   �Q             �Q               �� 
                 �Q  
         ��                            ����                            addMessage                              �R  �R      ��                  �  �  �R              L�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   4S              S               ��   \S             (S               ��                  PS           ��                            ����                            adjustTabOrder                              LT  4T      ��                  �  �  dT              ,N�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �T             |T  
             �� 
  �T             �T  
             ��                  �T           ��                            ����                            applyEntry                              �U  �U      ��                  �  �  �U              p!�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �U           ��                            ����                            changeCursor                                �V  �V      ��                  �  �  W              `�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                   W           ��                            ����                            createControls                              X  X      ��                  �  �  4X              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                                Y  Y      ��                  �  �  8Y              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                $Z  Z      ��                  �  �  <Z              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              0[  [      ��                  �  �  H[              pg�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              0\  \      ��                  �  �  H\              (h�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              0]  ]      ��                  �  �  H]               Ȳ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                8^   ^      ��                  �  �  P^              xȲ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              @_  (_      ��                  �  �  X_              x �                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �_             p_  
             ��   �_             �_               ��   �_             �_               ��                  �_           ��                            ����                            modifyUserLinks                             �`  �`      ��                  �  �  �`              P�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   Ha             a               ��   pa             <a               �� 
                 da  
         ��                            ����                            removeAllLinks                              `b  Hb      ��                  �  �  xb              <=�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              `c  Hc      ��                  �  �  xc              �?�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �c             �c  
             ��   �c             �c               �� 
                 �c  
         ��                            ����                            repositionObject                                �d  �d      ��                  �  �  �d              G�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   De             e               ��                  8e           ��                            ����                            returnFocus                             0f  f      ��                  �  �  Hf              �u�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 `f  
         ��                            ����                            showMessageProcedure                                dg  Lg      ��                  �  �  |g              (��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �g             �g               ��                  �g           ��                            ����                            toggleData                              �h  �h      ��                  �  �  �h              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �h           ��                            ����                            viewObject                              �i  �i      ��                  �  �  �i              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  |P      Lj      xj  � 
 ~      LOGICAL,    assignLinkProperty  Xj      �j      �j  �  �      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   �j      k      @k  �  �      CHARACTER,  getChildDataKey  k      Lk      |k  �  �      CHARACTER,  getContainerHandle  \k      �k      �k  �  �      HANDLE, getContainerHidden  �k      �k      �k  �  �      LOGICAL,    getContainerSource  �k      l      8l  �  �      HANDLE, getContainerSourceEvents    l      @l      |l  �  �      CHARACTER,  getContainerType    \l      �l      �l  �        CHARACTER,  getDataLinksEnabled �l      �l      �l  �        LOGICAL,    getDataSource   �l      m      8m  �  1      HANDLE, getDataSourceEvents m      @m      tm  �  ?      CHARACTER,  getDataSourceNames  Tm      �m      �m  �  S      CHARACTER,  getDataTarget   �m      �m      �m  �  f      CHARACTER,  getDataTargetEvents �m      �m      0n  �  t      CHARACTER,  getDBAware  n      <n      hn  � 
 �      LOGICAL,    getDesignDataObject Hn      tn      �n  �  �      CHARACTER,  getDynamicObject    �n      �n      �n  �  �      LOGICAL,    getInstanceProperties   �n      �n      ,o  �  �      CHARACTER,  getLogicalObjectName    o      8o      po  �  �      CHARACTER,  getLogicalVersion   Po      |o      �o  �  �      CHARACTER,  getObjectHidden �o      �o      �o  �  �      LOGICAL,    getObjectInitialized    �o      �o      0p  �        LOGICAL,    getObjectName   p      <p      lp  �        CHARACTER,  getObjectPage   Lp      xp      �p  �  (      INTEGER,    getObjectParent �p      �p      �p  �  6      HANDLE, getObjectVersion    �p      �p       q  �  F      CHARACTER,  getObjectVersionNumber   q      ,q      dq  �  W      CHARACTER,  getParentDataKey    Dq      pq      �q  �  n      CHARACTER,  getPassThroughLinks �q      �q      �q  �        CHARACTER,  getPhysicalObjectName   �q      �q      (r  �  �      CHARACTER,  getPhysicalVersion  r      4r      hr  �  �      CHARACTER,  getPropertyDialog   Hr      tr      �r  �  �      CHARACTER,  getQueryObject  �r      �r      �r  �  �      LOGICAL,    getRunAttribute �r      �r       s  �  �      CHARACTER,  getSupportedLinks    s      ,s      `s  �  �      CHARACTER,  getTranslatableProperties   @s      ls      �s  �  �      CHARACTER,  getUIBMode  �s      �s      �s  � 
       CHARACTER,  getUserProperty �s      �s      t  �  $      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    �s      Dt      |t  �  4      CHARACTER,INPUT pcPropList CHARACTER    linkHandles \t      �t      �t  �  I      CHARACTER,INPUT pcLink CHARACTER    linkProperty    �t      �t      $u  �  U      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry u      `u      �u  �  b      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   lu      �u      (v  �  n      CHARACTER,INPUT piMessage INTEGER   propertyType    v      Lv      |v  �  |      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  \v      �v      �v  �  �      CHARACTER,  setChildDataKey �v      �v      w  �  �      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  �v      8w      lw  �  �      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  Lw      �w      �w  �  �      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    �w      �w      x  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled �w      @x      tx  �  �      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   Tx      �x      �x  �  �      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents �x      �x       y  �  	      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames   y      Hy      |y  �        LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   \y      �y      �y  �  0      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents �y      �y      ,z  �  >      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  z      Pz      |z  � 
 R      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject \z      �z      �z  �  ]      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    �z      �z      ,{  �  q      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   {      H{      �{  �  �      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    `{      �{      �{  �  �      LOGICAL,INPUT c CHARACTER   setLogicalVersion   �{      �{      ,|  �  �      LOGICAL,INPUT cVersion CHARACTER    setObjectName   |      P|      �|  �  �      LOGICAL,INPUT pcName CHARACTER  setObjectParent `|      �|      �|  �  �      LOGICAL,INPUT phParent HANDLE   setObjectVersion    �|      �|      $}  �  �      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    }      L}      �}  �  �      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks `}      �}      �}  �  �      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   �}      �}      4~  �        LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  ~      T~      �~  �  )      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute h~      �~      �~  �  <      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   �~            8  �  L      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties         \      �  �  ^      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  x      �      �  � 
 x      LOGICAL,INPUT pcMode CHARACTER  setUserProperty �      �      8�  �  �      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage �      x�      ��  �  �      LOGICAL,INPUT pcMessage CHARACTER   Signature   ��      Ȁ      �  � 	 �      CHARACTER,INPUT pcName CHARACTER    �      4�  ��      |      4   ����|                ��                      ��                    4                  |N�                         D�          ܁  X�      �      4   �����                h�                      ��                  	  3                   O�                       	  �  h�       ��   �      �      4   �����                �                      ��                  ,  .                  ��                       ,  ��         -                                  |     
                    � ߱        ��  $  0  <�  ���                           $  2  ��  ���                       �                         � ߱        ��    8  �  ��      �      4   �����                ��                      ��                  9  �                  ��                       9  �  Ȅ  o   <      ,                                  �  $   =  �  ���                       L  @         8              � ߱        4�  �   >  l      H�  �   ?  �      \�  �   A  T      p�  �   C  �      ��  �   E  <      ��  �   G  �      ��  �   H  ,      ��  �   I  h      ԅ  �   L  �      �  �   N  P      ��  �   O  �      �  �   Q  H      $�  �   R  �      8�  �   S   	      L�  �   T  |	      `�  �   U  �	      t�  �   [  ,
      ��  �   ]  �
      ��  �   c  �
      ��  �   e  P      Ć  �   g  �      ؆  �   h  @      �  �   n  �       �  �   o  0      �  �   p  �      (�  �   q         <�  �   t  �      P�  �   u  �      d�  �   w  D      x�  �   x  �      ��  �   z  �      ��  �   {  0      ��  �   |  l      ȇ  �   }  �      ܇  �   ~  �      ��  �     `      �  �   �  �      �  �   �  �      ,�  �   �        @�  �   �  P      T�  �   �  �      h�  �   �  �      |�  �   �        ��  �   �  @          �   �  |                      ��          (�  �      ��                  $	  R	  @�              L!�                    O   ����    e�          O   ����    R�          O   ����    ��      �     
                h                     x                         � ߱        �  $ 8	  X�  ���                           O   P	  ��  ��  �               T�          D�  L�    4�                                             ��                            ����                                4      ��       �     6     \�                      V X�                       ��    r	  �  ��      �      4   �����                ��                      ��                  s	  �	                  i�                       s	  $�  ��  �   v	  $      ȋ  �   w	  �      ܋  �   x	        ��  �   y	  �      �  �   z	        �  �   {	  �      ,�  �   |	  �      @�  �   }	  x      T�  �   ~	  �      h�  �   	  p      |�  �   �	  �      ��  �   �	  `      ��  �   �	  �          �   �	  X      ��    
  Ԍ  P�      �      4   �����                `�                      ��                  
  �
                  �j�                       
  �  t�  �   
  (      ��  �   
  �      ��  �   	
        ��  �   

  �      č  �   
         ؍  �   
  t      �  �   
  �       �  �   
  d       �  �   
  �       (�  �   
  L!      <�  �   
  �!      P�  �   
  <"      d�  �   
  �"      x�  �   
  ,#      ��  �   
  �#      ��  �   
  $$      ��  �   
  �$      Ȏ  �   
  %      ܎  �   
  �%      ��  �   
  &      �  �   
  �&      �  �   
  '      ,�  �   
  �'      @�  �   
  (      T�  �   
  �(      h�  �    
  �(      |�  �   !
  x)          �   "
  �)      ��    �
  ��  (�      \*      4   ����\*                8�                      ��                  �
  Q                  ���                       �
  ��  L�  �   �
  �*      `�  �   �
  8+      t�  �   �
  �+      ��  �   �
  (,      ��  �   �
  �,      ��  �   �
  -      Đ  �   �
  �-      ؐ  �   �
  �-      �  �   �
  4.       �  �   �
  p.      �  �   �
  �.      (�  �   �
   /      <�  �   �
  �/      P�  �   �
  0      d�  �   �
  �0      x�  �   �
  �0      ��  �   �
  l1      ��  �   �
  �1      ��  �   �
  d2      ȑ  �   �
  �2      ܑ  �   �
  3      �  �   �
  �3      �  �   �
  �3      �  �   �
  84      ,�  �   �
  t4      @�  �   �
  �4      T�  �   �
  ,5      h�  �   �
  h5      |�  �   �
  �5      ��  �   �
  �5      ��  �   �
  6      ��  �   �
  X6      ̒  �   �
  �6      ��  �   �
  7      ��  �   �
  D7      �  �   �
  �7      �  �   �
  �7      0�  �   �
  �7      D�  �   �
  48      X�  �   �
  p8      l�  �   �
  �8      ��  �   �
   9      ��  �   �
  �9      ��  �   �
  :      ��  �   �
  |:      Г  �   �
  �:      �  �   �
  t;      ��  �   �
  �;      �  �   �
  l<       �  �   �
  �<      4�  �   �
  d=      H�  �   �
  �=      \�  �   �
  >      p�  �   �
  X>      ��  �   �
  �>      ��  �   �
  �>          �   �
  D?      �  $  ]  ؔ  ���                       �?     
                    � ߱        ��    �   �  0�      �?      4   �����?      /   �  \�     l�                          3   �����?            ��                      3   �����?  �    �  ��  4�   �  @      4   ����@  	              D�                      ��             	     �  %                  �E�                       �  ȕ  X�  �   �  l@      ��  $  �  ��  ���                       �@     
                    � ߱        Ė  �   �  �@      �  $   �  �  ���                       �@  @         �@              � ߱        ؗ  $  �  H�  ���                       4A                         � ߱        �A     
                $B                     tC  @        
 4C              � ߱        h�  V   �  t�  ���                        �C                     �C       	       	       �C                         � ߱        ��  $  �  �  ���                       �D     
                ,E                     |F  @        
 <F              � ߱        ��  V   �  ��  ���                        �F     
                G                     TH  @        
 H              � ߱            V   	  $�  ���                        
              �                      ��             
     '  �                  PG�                       '  ��  hH     
                �H                     4J  @        
 �I          �J  @        
 XJ          �J  @        
 �J          \K  @        
 K              � ߱            V   <  0�  ���                        adm-clone-props ��  �              �     7     `                          \  �                     start-super-proc    $�  ��  �           �     8                                  �                     ��    �  �  �      �N      4   �����N      /   �  H�     X�                          3   �����N            x�                      3   ����O  ��  $  �  ��  ���                       8O       
       
           � ߱        ��      ��  x�  �  TO      4   ����TO                �                      ��                                      ���                         �  hO       
       
       |O                     �O                         � ߱            $  	  ��  ���                               4�  p�      �O      4   �����O  �O       
       
           � ߱            $    D�  ���                       ��      ��  Ȟ   �  �O      4   �����O      $    ��  ���                       �O                         � ߱            �   3  P      PP     
                �P                     R  @        
 �Q              � ߱        ğ  V   G  4�  ���                        ؟  �   z  (R      p�    �  ��  �      hR      4   ����hR      /   �  0�     @�                          3   ����xR            `�                      3   �����R  ,�  $    ��  ���                       �R                         � ߱        �R     
                \S                     �T  @        
 lT              � ߱        X�  V     Ƞ  ���                        8�    �  t�  �      �T      4   �����T                 �                      ��                  �  �                  ��                       �  ��      g   �  �         +�ܣ                           �          ��  ��      ��                  �      Ȣ              ���                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  �     �  �T                      3   �����T  L�     
   <�                      3   �����T         
   l�                      3   �����T    ��                              ��        `                  ����                                        ,�              9      |�                      g                               @�  g   �  P�          +�	�                           �          �  Ф      ��                  �  �   �              ��                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  D�     T�  U                      3   �����T            t�                      3   ���� U    ��                              ��        `                  ����                                        d�              :      ��                      g                               H�  g   �  X�          +�	�                            �          �  ئ      ��                  �  �  �              ���                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  L�     \�  XU                      3   ����<U            |�                      3   ����`U    ��                              ��        `                  ����                                        l�              ;      ��                      g                               ��    �  d�  �      |U      4   ����|U                �                      ��                  �  �                  `��                       �  t�  \�  /   �  �     ,�                          3   �����U            L�                      3   �����U  X�  /  �  ��     ��  �U                      3   �����U  ȩ     
   ��                      3   �����U  ��        �                      3   �����U  (�        �                      3   ����V            H�                      3   ����0V  ��    �  t�  ��      TV      4   ����TV      /  �  ��     ��  �V                      3   �����V  �     
   �                      3   �����V   �        �                      3   �����V  P�        @�                      3   ���� W            p�                      3   ����$W        �  ��  ��      DW      4   ����DW      /  �  ث     �  �W                      3   ����xW  �     
   �                      3   �����W  H�        8�                      3   �����W  x�        h�                      3   �����W            ��                      3   �����W  @�     �  �W                                     X     
                �X                     �Y  @        
 �Y              � ߱        Э  V   C  ܬ  ���                        �Y     
                lZ                     �[  @        
 |[              � ߱        D�  V   j  l�  ���                        �[  @         �[          \  @         �[              � ߱        p�  $   �  ��  ���                       $�  g   �  ��         +6ȯ                            P�           �  �      ��                  �  �  8�              ;�                    O   ����    e�          O   ����    R�          O   ����    ��            �   \  }        ��                              ��        `                  ����                                        ��              <      h�                      g                               t�    �  @�  ��      8\      4   ����8\                ̰                      ��                  �  �                  �!�                       �  P�  �  	  �   �                                        3   ����L\  L�  /   �  <�                                 3   �����\  \�  �   �  �\      O   �  ��  ��  �\  ��    �  ��  ��      �\      4   �����\      $   �  ̱  ���                       L]  @         8]              � ߱        ��  /   �  $�                                 3   ����T]                �          Ȳ  ��      ��                 �  �                  �V�                P�     �  4�      O   �    ��          O   �    ��      �  /   �  �                                 3   ����p]      k   �  8�                    &�        �       /   �  |�                                 3   �����]  adm-create-objects  ��  ��                      =      �                               �                     disable_UI  ��  ��                      >      �                               �  
                   enable_UI   �  d�                      ?      �             (              �  	                    ��� ���  �                8   ����       8   ����       �   �      toggleData  ,INPUT plEnabled LOGICAL    �  L�  d�      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  <�  ��  ��      returnFocus ,INPUT hTarget HANDLE   ��  ܵ  �      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    ̵  ,�  8�      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE �  ��  ��      removeAllLinks  ,   |�  ��  ��      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE ��  �  ,�      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    �  ��  ��      hideObject  ,   ��  ķ  з      exitObject  ,   ��  �  ��      editInstanceProperties  ,   Է  �   �      displayLinks    ,    �  4�  D�      createControls  ,   $�  X�  h�      changeCursor    ,INPUT pcCursor CHARACTER   H�  ��  ��      applyEntry  ,INPUT pcField CHARACTER    ��  ̸  ܸ      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER ��  4�  @�      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER $�  ��  ��      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  ��  �      unbindServer    ,INPUT pcMode CHARACTER �  ,�  @�      startServerObject   ,   �  T�  d�      runServerObject ,INPUT phAppService HANDLE  D�  ��  ��      restartServerObject ,   ��  ��  к      initializeServerObject  ,   ��  �  ��      disconnectObject    ,   Ժ  �   �      destroyServerObject ,   ��  4�  @�      bindServer  ,   $�  T�  d�      processAction   ,INPUT pcAction CHARACTER   D�  ��  ��      enableObject    ,   ��  ��  Ļ      disableObject   ,   ��  ػ  �      applyLayout ,   Ȼ  ��  �      viewPage    ,INPUT piPageNum INTEGER    �  0�  <�      viewObject  ,    �  P�  X�      toolbar ,INPUT pcValue CHARACTER    @�  ��  ��      selectPage  ,INPUT piPageNum INTEGER    t�  ��  м      removePageNTarget   ,INPUT phTarget HANDLE,INPUT piPage INTEGER ��  �  �      passThrough ,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER  ��  `�  l�      notifyPage  ,INPUT pcProc CHARACTER P�  ��  ��      initPages   ,INPUT pcPageList CHARACTER ��  ̽  �      initializeVisualContainer   ,   ��  ��  �      initializeObject    ,   �  $�  0�      hidePage    ,INPUT piPageNum INTEGER    �  \�  l�      destroyObject   ,   L�  ��  ��      deletePage  ,INPUT piPageNum INTEGER    p�  ��  Ⱦ      createObjects   ,   ��  ܾ  �      constructObject ,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE ̾  `�  l�      confirmExit ,INPUT-OUTPUT plCancel LOGICAL  P�  ��  ��      changePage  ,   ��  ��  п      assignPageProperty  ,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER      � 
"     
 �%     adecomm/as-utils.w 
"   
   �    }        �
"     
   "      "          
�    
�        �     }         �     }        �     }             �     }        �%                  �     }         �     }        �     }             �     }        �%              � 
"    
 �%              � ��  �         �      \     H     $              
�    � �   �     
�             �G� �   �G     
�             �G                      
�            � �     
"    
 �
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        ,    7%               
"   
 ��           `    1� �  
 �� �   �%               o%   o           � �    �
"   
 ��           �    1� �   �� �   �%               o%   o           � �   �
"   
 ��           H    1� �  
 �� �   �%               o%   o           � �   �
"   
 ��           �    1� �   �� �   �%               o%   o           �   
 �
"   
 ��           0    1�    �� �   �%               o%   o           �     �
"   
 ��           �    1� 7   �� C   �%               o%   o           %               
"   
 ��               1� K   �� [     
"   
 ��           \    1� b   �� �   �%               o%   o           � u  e �
"   
 ��           �    1� �   �� �   �%               o%   o           � �  ? �
"   
 ��           D    1� *   �� C   �%               o%   o           %               
"   
 ��           �    1� :   �� C   �%               o%   o           %               
"   
 ��           <    1� L   �� C   �%               o%   o           %              
"   
 ��          �    1� Y   �� C     
"   
 ��           �    1� h  
 �� C   �%               o%   o           %               
"   
 ��           p	    1� s   �� �   �%               o%   o           � �    �
"   
 ��          �	    1� {   �� [     
"   
 ��            
    1� �   �� �   �%               o%   o           � �  t �
"   
 ��          �
    1�   
 �� [     
"   
 ��           �
    1� !   �� �   �%               o%   o           � 2  � �
"   
 ��           D    1� �   �� �   �%               o%   o           � �    �
"   
 ��           �    1� �  
 �� �   �%               o%   o           %               
"   
 ��           4    1� �   �� C   �%               o%   o           %               
"   
 ��           �    1� �   �� �   �%               o%   o           � �    �
"   
 ��           $    1� �   �� �   �%               o%   o           o%   o           
"   
 ��           �    1�   
 �� �   �%               o%   o           � �    �
"   
 ��               1�    �� *  	 �%               o%   o           � 4  / �
"   
 ��          �    1� d   �� *  	   
"   
 ��           �    1� v   �� *  	 �o%   o           o%   o           � �    �
"   
 ��          8    1� �   �� *  	   
"   
 ��           t    1� �   �� *  	 �o%   o           o%   o           � �    �
"   
 ��          �    1� �   �� C     
"   
 ��          $    1� �   �� *  	   
"   
 ��          `    1� �   �� *  	   
"   
 ��          �    1� �   �� *  	   
"   
 ��           �    1� �   �� C   �o%   o           o%   o           %              
"   
 ��          T    1� �   �� *  	   
"   
 ��          �    1� �  
 ��      
"   
 ��          �    1�    �� *  	   
"   
 ��              1�    �� *  	   
"   
 ��          D    1� 2   �� *  	   
"   
 ��          �    1� G   �� *  	   
"   
 ��          �    1� V  	 �� *  	   
"   
 ��          �    1� `   �� *  	   
"   
 ��          4    1� s   �� *  	   
"   
 ��           p    1� �   �� �   �%               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 �
"   
   
"   
 �(�  L ( l       �        8    �� �   � P   �        D    �@    
� @  , 
�       P    �� �     p�               �L
�    %              � 8      \    � $         � �          
�    � �     
"   
 �� @  , 
�       l    �� �  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 ��               1� �  
 �� �   �%               o%   o           � �    �
"   
 ��           �    1� �  
 �� �   �%               o%   o           o%   o           
"   
 ��               1� �   �� [   �%               o%   o           o%   o           
"   
 ��           �    1� �   �� C   �%               o%   o           %               
"   
 ��                1� �   �� C   �%               o%   o           %               
"   
 ��           |    1� �   �� �   �%               o%   o           � �    �
"   
 ��           �    1�    �� C   �%               o%   o           %              
"   
 ��           l    1�    �� C   �%               o%   o           o%   o           
"   
 ��           �    1� #   �� �   �%               o%   o           o%   o           
"   
 ��           d    1� 1  	 �� �   �%               o%   o           � �    �
"   
 ��           �    1� ;   �� �   �%               o%   o           o%   o           
"   
 ��           T    1� O   �� �   �%               o%   o           o%   o           
"   
 ��           �    1� ^   �� C   �%               o%   o           %               
"   
 ��           L    1� n   �� C   �%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 ��               1� z   �� *  	 �%               o%   o           � �    �
"   
 ��           �    1� �   �� *  	 �%               o%   o           � �    �
"   
 ��               1� �   �� C   �%               o%   o           %               
"   
 ��           �    1� �   �� *  	 �%               o%   o           � �    �
"   
 ��           �    1� �   �� *  	 �%               o%   o           � �    �
"   
 ��           h    1� �   �� C   �%               o%   o           %               
"   
 ��           �    1� �   �� *  	 �%               o%   o           � �    �
"   
 ��           X     1� �   �� *  	 �%               o%   o           � �    �
"   
 ��           �     1� �   �� *  	 �%               o%   o           � �    �
"   
 ��           @!    1� �   �� *  	 �%               o%   o           o%   o           
"   
 ��           �!    1�    �� *  	 �%               o%   o           � �    �
"   
 ��           0"    1�    �� *  	 �%               o%   o           � �    �
"   
 ��           �"    1� &  	 ��    �%               o%   o           %               
"   
 ��            #    1� 0   ��    �%               o%   o           %               
"   
 ��           �#    1� 9   �� C   �%               o%   o           o%   o           
"   
 ��           $    1� J   �� C   �%               o%   o           o%   o           
"   
 ��           �$    1� Y   �� C   �%               o%   o           %               
"   
 ��           %    1� g   �� C   �%               o%   o           %               
"   
 ��           �%    1� x   �� C   �%               o%   o           %               
"   
 ��           &    1� �   �� �   �%               o%   o           %       
       
"   
 ��           �&    1� �   �� �   �%               o%   o           o%   o           
"   
 ��            '    1� �   �� �   �%               o%   o           %              
"   
 ��           |'    1� �   �� �   �%               o%   o           o%   o           
"   
 ��           �'    1� �   �� �   �%               o%   o           %              
"   
 ��           t(    1� �   �� �   �%               o%   o           o%   o           
"   
 ��           �(    1� �   �� �   �%               o%   o           %              
"   
 ��           l)    1� �   �� �   �%               o%   o           o%   o           
"   
 ��           �)    1� �   �� *  	 �%               o%   o           � �    �P �L 
�H T   %              �     }        �GG %              
"   
 ��           �*    1�    �� �   �%               o%   o           %               
"   
 ��           ,+    1�    �� �   �%               o%   o           o%   o           
"   
 ��           �+    1�    �� �   �%               o%   o           � �    �
"   
 ��           ,    1� )   �� �   �%               o%   o           � ?  - �
"   
 ��           �,    1� m   �� �   �%               o%   o           � �    �
"   
 ��           -    1� �   �� �   �%               o%   o           � �   �
"   
 ��          x-    1� �   �� [     
"   
 ��           �-    1� �   �� �   �%               o%   o           � �    �
"   
 ��          (.    1� �  
 �� [     
"   
 ��          d.    1� �   �� [     
"   
 ��           �.    1� �   �� *  	 �%               o%   o           � �    �
"   
 ��           /    1�    �� �   �%               o%   o           � �    �
"   
 ��           �/    1�    �� [   �%               o%   o           o%   o           
"   
 ��           0    1�    �� �   �%               o%   o           � .  ! �
"   
 ��           x0    1� P   �� �   �%               o%   o           � �    �
"   
 ��           �0    1� ]   �� �   �%               o%   o           � p   �
"   
 ��           `1    1�   	 �� �   �%               o%   o           o%   o           
"   
 ��           �1    1� �   �� C   �%               o%   o           %               
"   
 ��          X2    1� �   �� [     
"   
 ��           �2    1� �   �� �   �%               o%   o           � �   �
"   
 ��           3    1� �   �� *  	 �%               o%   o           � �    �
"   
 ��           |3    1� �   �� *  	 �%               o%   o           � �    �
"   
 ��          �3    1� �   �� [     
"   
 ��          ,4    1� �   �� *  	   
"   
 ��           h4    1�    �� C   �o%   o           o%   o           %               
"   
 ��          �4    1�    �� C     
"   
 ��           5    1� 6   �� *  	   
"   
 ��          \5    1� D   �� *  	   
"   
 ��          �5    1� W   �� *  	   
"   
 ��          �5    1� h   �� *  	   
"   
 ��          6    1� y   �� *  	   
"   
 ��          L6    1� �   �� [     
"   
 ��           �6    1� �   �� �   �%               o%   o           � �  4 �
"   
 ��          �6    1� �   �� [     
"   
 ��          87    1� �   �� [     
"   
 ��          t7    1�    �� [     
"   
 ��          �7    1�    �� *  	   
"   
 ��          �7    1� %   �� *  	   
"   
 ��          (8    1� 7   �� *  	   
"   
 ��          d8    1� I   �� C     
"   
 ��           �8    1� V   �� *  	 �%               o%   o           � �    �
"   
 ��           9    1� d   �� *  	 �%               o%   o           � �    �
"   
 ��           �9    1� p   �� *  	 �%               o%   o           � �    �
"   
 ��           �9    1� �   �� *  	 �%               o%   o           � �    �
"   
 ��           p:    1� �   �� C   �%               o%   o           %               
"   
 ��           �:    1� �   �� C   �%               o%   o           o%   o           
"   
 ��           h;    1� �   �� C   �%               o%   o           %               
"   
 ��           �;    1� �   �� C   �%               o%   o           %               
"   
 ��           `<    1� �   �� C   �%               o%   o           o%   o           
"   
 ��           �<    1� �   �� C   �%               o%   o           %               
"   
 ��          X=    1� �   �� *  	   
"   
 ��           �=    1�    �� C   �%               o%   o           %              
"   
 ��          >    1�    �� *  	   
"   
 ��          L>    1� *   �� *  	   
"   
 ��          �>    1� 9  
 �� *  	   
"   
 ��           �>    1� D   �� *  	 �%               o%   o           � �   �
"   
 ��           8?    1� V   �� *  	 �%               o%   o           � �    �
�             �G "    �%     start-super-proc ��%     adm2/smart.p +�P �L 
�H T   %              �     }        �GG %              
"   
   �       `@    6� �     
"   
   
�        �@    8
"   
   �        �@    ��     }        �G 4              
"   
 ߱G %              G %              %p e `   LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout �
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �A    �� �   � P   �         B    �@    
� @  , 
�       B    �� �   �p�               �L
�    %              � 8      B    � $         � �          
�    � �   �
"   
 �p� @  , 
�       (C    �� b   �p�               �L"    , �   � �   �� �   ��     }        �A      |    "      � �   �%              (<   \ (    |    �     }        �A� �   �A"  	  �    "    �"  	  �  < "    �"  	  �(    |    �     }        �A� �   �A"  	  �
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �D    �� �   � P   �        E    �@    
� @  , 
�       E    �� �   �p�               �L
�    %              � 8       E    � $         � �          
�    � �   �
"   
 �p� @  , 
�       0F    �� �  
 �p�               �L"    , 
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 �(�  L ( l       �        �F    �� �   � P   �        �F    �@    
� @  , 
�       �F    �� �   �p�               �L
�    %              � 8      �F    � $         � �   �     
�    � �   �
"   
 �p� @  , 
�       H    �� K   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
   
"   
 �
"   
   
"   
   (�  L ( l       �        �H    �� �   � P   �        �H    �@    
� @  , 
�       �H    �� �     p�               �L
�    %              � 8      �H    � $         � �          
�    � �     
"   
 �p� @  , 
�       �I    �� �  
 �p�               �L%     SmartDialog 
"   
   p� @  , 
�       LJ    �� �     p�               �L% 
    DIALOG-BOX  
"   
  p� @  , 
�       �J    �� �    p�               �L%               
"   
  p� @  , 
�       K    �� v    p�               �L(        � �      � �      � �      �     }        �A
�H T   %              �     }        �GG %              
"   
 � (   � 
"   
 �    �        �K    �� �   �
"   
   � 8      <L    � $         � �          
�    � �   �
"   
   �        �L    �
"   
   �       �L    /
"   
   
"   
   �       �L    6� �     
"   
   
�        M    8
"   
   �        ,M    �
"   
   �       LM    �
"   
   p�    � �   �
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 �    �        N    �A"    �A
"   
   
�        \N    �@ � 
"   
 �"      �       }        �
"   
 �%              %                "    �%     start-super-proc ��%     adm2/appserver.p ���    � A     
�    �     }        �%               %      Server  - �     }        �    "  
  �� �    �%                   "    �� �    �%      NONE    p�,  8         $     "    �        � [   �
�    
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �P    �� �   � P   �        �P    �@    
� @  , 
�       �P    �� �   �p�               �L
�    %              � 8      �P    � $         � �          
�    � �   �
"   
 �p� @  , 
�       �Q    �� ;   �p�               �L"    , p�,  8         $     "  
  �        � i   �
�     "    �%     start-super-proc ��%     adm2/visual.p ��   � �     � �     � �     
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        ,S    �� �   � P   �        8S    �@    
� @  , 
�       DS    �� �   �p�               �L
�    %              � 8      PS    � $         � �          
�    � �   �
"   
 �p� @  , 
�       `T    �� �   �p�               �L"    , � 
"    
 �%     contextHelp 
"    
   
�    
�    %     processAction   
�    %     CTRL-PAGE-UP +�%     processAction   
�    %     CTRL-PAGE-DOWN  "    �%     start-super-proc �%     adm2/containr.p %     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents �%      initializeDataObjects �0 0   A    �    � �   �
�    � �   �A    �    � �     
�    �    �%     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents �%     buildDataRequest ent0 A    �    � �   �
�    �     �%     modifyListProperty 
�    
�    %      Add     %     SupportedLinks %      ContainerToolbar-Target %               
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 �(�  L ( l       �        \X    �� �   � P   �        hX    �@    
� @  , 
�       tX    �� �   �p�               �L
�    %              � 8      �X    � $         � �   �     
�    � �   �
"   
 �p� @  , 
�       �Y    �� �   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 �(�  L ( l       �        <Z    �� �   � P   �        HZ    �@    
� @  , 
�       TZ    �� �   �p�               �L
�    %              � 8      `Z    � $         � �   �     
�    � �   �
"   
 �p� @  , 
�       p[    �� �   �p�               �L%              �             I%               �             �%              % 	    END-ERROR ��     }        � `     @     ,         � e  (   G %       
       � �  &   G %       
       � �  & �% 
    disable_UI 
�    %                0   � 
�        
�             � 
%   
           
�             � 
�    %     createObjects   %     initializeObject �%     destroyObject   "      "                      �           �   l       ��                 4  X  �               ���                    O   ����    e�          O   ����    R�          O   ����    ��        $  C  �   ���                       �K     
                    � ߱              D  (  �      �K      4   �����K                �                      ��                  E  W                  ���                       E  8  �  �  F  HL            H  �  `      �L      4   �����L                p                      ��                  I  V                  ��                       I  �  �  o   J      ,                                 �  �   K  �L      �  �   L  �L      $  $  M  �  ���                       M     
                    � ߱        8  �   N  8M      L  �   O  XM      `  �   R  xM          $   U  �  ���                       �M  @         �M              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 |  �  �               �~�                    O   ����    e�          O   ����    R�          O   ����    ��      �                      �          �  $  �    ���                       �M     
                    � ߱                  �  �                      ��                   �  �                  �M�                     �  4      4   ����N      $  �  �  ���                       hN     
                    � ߱        �    �  4  D      |N      4   ����|N      /  �  p                               3   �����N  �  �   �  �N          O   �  ��  ��  �N                               , �                          
                               �      ��                            ����                                                        �   l       ��                      �               `W�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                                            �           �   l       ��                      �               G�                    O   ����    e�          O   ����    R�          O   ����    ��               �� �                   ��                              ��        `                  ����                                            �           �   l       ��                    -  �               �I�                    O   ����    e�          O   ����    R�          O   ����    ��          �               �              � ߱          h   )  �    �                        4  
   +  �� ,                    s   ,  `                 �              �  �       ��                            7   ����           ��                     �            ,                  6   ,         P   ��                    �            ,                                                                �  �                                   @            l   |          �]  �]          �    ��                              ��        `                  ����                            L        2                 ��        pP          L  �
   ��                              
 �                                                                          U         
                                    
 �                                                                   $      Z  (                                             �                                                                                                                                       ;    d d     �   ��  �  � �       h  �                                  `   x                                                            
   d     D                                                                 H  �� pP                                  L          �           \  ��s                                 E                                  A       D                                                                                                        TXS appSrvUtils T-ALM CodCia CodAlm Descripcion TdoArt AutMov DirAlm HorRec EncAlm TelAlm CorrSal CorrIng CorrTrf CodDiv Clave AlmCsg TpoCsg CodCli flgrep Campo-C AlmPrincipal AlmDespacho Campo-Log ASSIGNFOCUSEDWIDGET ASSIGNWIDGETVALUE ASSIGNWIDGETVALUELIST BLANKWIDGET CLEARWIDGET DISABLERADIOBUTTON DISABLEWIDGET ENABLERADIOBUTTON ENABLEWIDGET FORMATTEDWIDGETVALUE FORMATTEDWIDGETVALUELIST HIDEWIDGET HIGHLIGHTWIDGET RESETWIDGETVALUE TOGGLEWIDGET VIEWWIDGET WIDGETHANDLE WIDGETLONGCHARVALUE WIDGETISBLANK WIDGETISFOCUSED WIDGETISMODIFIED WIDGETISTRUE WIDGETVALUE WIDGETVALUELIST Btn_OK BROWSE-2 x(3) X(40) gDialog SELECCIONE EL ALMACEN DE DESPACHO DISABLEPAGESINFOLDER ENABLEPAGESINFOLDER GETCALLERPROCEDURE GETCALLERWINDOW GETCONTAINERMODE GETCONTAINERTARGET GETCONTAINERTARGETEVENTS GETCURRENTPAGE GETDISABLEDADDMODETABS GETDYNAMICSDOPROCEDURE GETFILTERSOURCE GETMULTIINSTANCEACTIVATED GETMULTIINSTANCESUPPORTED GETNAVIGATIONSOURCE GETNAVIGATIONSOURCEEVENTS GETNAVIGATIONTARGET GETOUTMESSAGETARGET GETPAGENTARGET GETPAGESOURCE GETPRIMARYSDOTARGET GETREENABLEDATALINKS GETRUNDOOPTIONS GETRUNMULTIPLE GETSAVEDCONTAINERMODE GETSDOFOREIGNFIELDS GETTOPONLY GETUPDATESOURCE GETUPDATETARGET GETWAITFOROBJECT GETWINDOWTITLEVIEWER GETSTATUSAREA PAGENTARGETS SETCALLEROBJECT SETCALLERPROCEDURE SETCALLERWINDOW SETCONTAINERMODE SETCONTAINERTARGET SETCURRENTPAGE SETDISABLEDADDMODETABS SETDYNAMICSDOPROCEDURE SETFILTERSOURCE SETINMESSAGETARGET SETMULTIINSTANCEACTIVATED SETMULTIINSTANCESUPPORTED SETNAVIGATIONSOURCE SETNAVIGATIONSOURCEEVENTS SETNAVIGATIONTARGET SETOUTMESSAGETARGET SETPAGENTARGET SETPAGESOURCE SETPRIMARYSDOTARGET SETREENABLEDATALINKS SETROUTERTARGET SETRUNDOOPTIONS SETRUNMULTIPLE SETSAVEDCONTAINERMODE SETSDOFOREIGNFIELDS SETTOPONLY SETUPDATESOURCE SETUPDATETARGET SETWAITFOROBJECT SETWINDOWTITLEVIEWER GETOBJECTTYPE SETSTATUSAREA GETALLFIELDHANDLES GETALLFIELDNAMES GETCOL GETDEFAULTLAYOUT GETDISABLEONINIT GETENABLEDOBJFLDS GETENABLEDOBJHDLS GETHEIGHT GETHIDEONINIT GETLAYOUTOPTIONS GETLAYOUTVARIABLE GETOBJECTENABLED GETOBJECTLAYOUT GETROW GETWIDTH GETRESIZEHORIZONTAL GETRESIZEVERTICAL SETALLFIELDHANDLES SETALLFIELDNAMES SETDEFAULTLAYOUT SETDISABLEONINIT SETHIDEONINIT SETLAYOUTOPTIONS SETOBJECTLAYOUT SETRESIZEHORIZONTAL SETRESIZEVERTICAL GETOBJECTTRANSLATED GETOBJECTSECURED CREATEUIEVENTS GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE , prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDialog ContainerType DIALOG-BOX PropertyDialog adm2/support/visuald.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout SupportedLinks Data-Target,Data-Source,Page-Target,Update-Source,Update-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext ObjectLayout LayoutOptions ObjectEnabled LayoutVariable DefaultLayout DisableOnInit EnabledObjFlds EnabledObjHdls FieldSecurity SecuredTokens AllFieldHandles AllFieldNames MinHeight MinWidth ResizeHorizontal ResizeVertical ObjectSecured ObjectTranslated PopupButtonsInFields ColorInfoBG INTEGER ColorInfoFG ColorWarnBG ColorWarnFG ColorErrorBG ColorErrorFG BGColor FGColor FieldPopupMapping CurrentPage PendingPage ContainerTarget ContainerTargetEvents exitObject,okObject,cancelObject,updateActive ContainerToolbarSource ContainerToolbarSourceEvents toolbar,okObject,cancelObject OutMessageTarget PageNTarget PageSource FilterSource UpdateSource UpdateTarget CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState StartPage RunMultiple WaitForObject DynamicSDOProcedure adm2/dyndata.w RunDOOptions InitialPageList WindowFrameHandle Page0LayoutManager MultiInstanceSupported MultiInstanceActivated ContainerMode SavedContainerMode SdoForeignFields NavigationSource NavigationTarget PrimarySdoTarget NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter CallerWindow CallerProcedure CallerObject DisabledAddModeTabs ReEnableDataLinks WindowTitleViewer UpdateActive InstanceNames ClientNames ContainedDataObjects ContainedAppServices DataContainer HasDbAwareObjects HasDynamicProxy HideOnClose HideChildContainersOnClose HasObjectMenu RequiredPages RemoveMenuOnHide ProcessList PageLayoutInfo PageTokens DataContainerName WidgetIDFileName ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService cFields adm2/visual.p   BROWSE-2 Btn_OK CTRL-PAGE-UP CTRL-PAGE-DOWN adm2/containr.p Add initializeDataObjects getSupportedLinks data-target data-source buildDataRequest containertoolbar-target ContainerToolbar-Target END-ERROR iStartPage A SmartDialog is not intended to be run  Persistent or to be placed in another  SmartObject at AppBuilder design time. ADM-CREATE-OBJECTS DISABLE_UI ENABLE_UI alm01 Almac�n Descripci�n OK d
    �
  �#      % �8   ��      0         pcProp      ��      P         pcProp      ��      p         plCancel    �   ��      �         pcProcName  �   ��      �        
 pcProcName  �   ��      �         pcProcName      ��              
 pcProcName      ��      $        piPageNum       ��      H        piPageNum       ��      l        pcPageList      ��      �        pcProc  �  ��      �        pcLinkName      ��      �        pcLinkName    ��      �       
 phTarget        ��              phTarget        ��      @        piPageNum       ��      d        pcValue     ��      �        piPageNum       ��      �        pcAction        ��      �       
 phAppService        ��      �        pcMode     ��             
 phSource    D  ��      8        phSource        ��      \       
 phSource    �  ��      �        pcText  �  ��      �        pcText      ��      �        pcText  �  ��      �       
 phObject      ��             
 phObject        ��      (        phObject        ��      L        pcField     ��      l        pcCursor    �  ��      �       
 phCaller    �  ��      �        phCaller    �  ��      �        phCaller        ��      �        phCaller    (  ��               pcMod   H  ��      @        pcMod       ��      `       
 pcMod   �  ��      �       
 phSource    �  ��      �        phSource        ��      �       
 phSource    �  ��      �        pdRow       ��              pdRow       ��      ,       
 hTarget X  ��      L        pcMessage       ��      p        pcMessage       ��      �        plEnabled             �     cType       �     6   �          �                  getObjectType   8	  P	  R	  ,          
   hReposBuffer    L        @  
   hPropTable  h        `  
   hBuffer           |  
   hTable  �  �     7             �                  adm-clone-props C  D  E  F  H  I  J  K  L  M  N  O  R  U  V  W  X              
   hProc             <        pcProcName  �  �  	   8     $      x                  start-super-proc    �  �  �  �  �  �  �  �  �  H  �     9                                   �  �  	     :                                   �  �  �  L	     ;                                   �  �  	  �	     <                                   �  �  T	  �	     =               �	                  adm-create-objects    �	  
     >               
                  disable_UI      �	  T
     ?               H
                  enable_UI   )  +  ,  -  
  �  �     
 x      �                          �
  �
     T-ALM   �         �         �         �         �         �         �         �         �                                                        (         0         8         @         H  
      P         `         l  
      CodAlm  Descripcion CodCia  TdoArt  AutMov  DirAlm  HorRec  EncAlm  TelAlm  CorrSal CorrIng CorrTrf CodDiv  Clave   AlmCsg  TpoCsg  CodCli  flgrep  Campo-C AlmPrincipal    AlmDespacho Campo-Log   �          �  
   appSrvUtils �        �  
   gshAstraAppserver   �        �  
   gshSessionManager           �  
   gshRIManager    4           
   gshSecurityManager  \        H  
   gshProfileManager   �        p  
   gshRepositoryManager    �  	 	     �  
   gshTranslationManager   �  
 
     �  
   gshWebManager   �        �     gscSessionId                  gsdSessionObj   D        4  
   gshFinManager   h        X  
   gshGenManager   �        |  
   gshAgnManager   �        �     gsdTempUniqueID �        �     gsdUserObj  �        �     gsdRenderTypeObj                  gsdSessionScopeObj  <       4  
   ghProp  \       P  
   ghADMProps  �       p  
   ghADMPropsBuf   �       �     glADMLoadFromRepos  �       �     glADMOk �       �  
   ghContainer        �     cObjectName      	        iStart  @    
   4     cAppService `       T     cASDivision �       t     cServerOperatingMode    �       �     cFields          �     iStartPage        X  �  T-ALM            C   �  �  �  �  �  �  �        	     ,  -  .  0  2  3  4  8  9  <  =  >  ?  A  C  E  G  H  I  L  N  O  Q  R  S  T  U  [  ]  c  e  g  h  n  o  p  q  t  u  w  x  z  {  |  }  ~    �  �  �  �  �  �  �  �  �  �  r	  s	  v	  w	  x	  y	  z	  {	  |	  }	  ~	  	  �	  �	  �	  �	  �	  
  
  
  
  	
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   
  !
  "
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
  Q  ]  �  �  �  �  �  �  �  �  �  �  �  �  	  %  '  <  �  �  �  �      	            3  G  z  �  �      �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  C  j  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �      �� $ C:\Progress\OpenEdge\src\adm2\dialogmn.i �  f!  C:\Progress\OpenEdge\src\adm2\containr.i    � # %C:\Progress\OpenEdge\src\adm2\custom\containrcustom.i    4  ��  C:\Progress\OpenEdge\src\adm2\visual.i   x  # " %C:\Progress\OpenEdge\src\adm2\custom\visualcustom.i  �  �<  C:\Progress\OpenEdge\src\adm2\appserver.i    �  �� ! %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   $  I�  C:\Progress\OpenEdge\src\adm2\smart.i    h  Ds   C:\Progress\OpenEdge\gui\fn  �  tw  %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   �  Q.  C:\Progress\OpenEdge\gui\set   ��  C:\Progress\OpenEdge\src\adm2\cntnprop.i ,  ��  %C:\Progress\OpenEdge\src\adm2\custom\cntnpropcustom.i    `  P  %C:\Progress\OpenEdge\src\adm2\custom\cntnprtocustom.i    �  F>  C:\Progress\OpenEdge\src\adm2\visprop.i  �  �I  %C:\Progress\OpenEdge\src\adm2\custom\vispropcustom.i   ��  %C:\Progress\OpenEdge\src\adm2\custom\visprtocustom.i \  �l 
 C:\Progress\OpenEdge\src\adm2\appsprop.i �  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    �  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i      i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i X  �j  C:\Progress\OpenEdge\gui\get �  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    �  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    �  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i <  Su  C:\Progress\OpenEdge\src\adm2\globals.i  p  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i �  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   �  �  C:\Progress\OpenEdge\src\adm2\appsprto.i (  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   \  �X 	 C:\Progress\OpenEdge\src\adm2\visprto.i  �  !�  %C:\Progress\OpenEdge\src\adm2\custom\visualdefscustom.i  �  n�  C:\Progress\OpenEdge\src\adm2\cntnprto.i   ;  %C:\Progress\OpenEdge\src\adm2\custom\containrdefscustom.i    P  ~�  C:\Progress\OpenEdge\src\adm2\widgetprto.i   �  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   �  �    d:\newsie\on_in_co\APLIC\vtaexp\d-almdes-exp.w       �   �      D     �  $   T  �   �      d  �   �     t     b     �  �   ]     �     ;     �  �   3     �     �  #   �  �   �     �     �      �  �   �     �     �        �   �          �      $  r   �     4  n   �     D     *  "   T  i   %     d          t  P   �     �  �   �     �     �  !   �  �   �     �     b     �  �   a     �     ?     �  �   =     �            g             �     $  O   �     4  �   T     D     R      T  �   "     d     �     t  �   �     �     �     �  �   �     �     z     �  �   y     �     W     �  �   V     �     4     �  �   #                   �   �     $      �     4   }   �     D      �     T      2     d      �     t      �     �   7   Z     �   �   Q     �   O   C     �      2     �      �
     �   �   �
     �   �   �
     �   O   �
     !     t
     !     &
     $!  �   
     4!  x   �	  
   D!  M   �	     T!     �	     d!     �	     t!  a   p	  
   �!  �  O	     �!     0	     �!  �  �     �!  O   �     �!     �     �!     �     �!  �   �     �!     �     "     �     "  x   �     $"     �     4"     K     D"     G     T"     3     d"          t"  Q   
  
   �"     �     �"     x  
   �"     d     �"     J  
   �"  f        �"     �  	   �"  "   z     �"     f     #     E     #  Z   �     $#     �     4#     �     D#     �     T#     �     d#     Y     t#  3   �       �#     L      �#  	   "       �#     	      