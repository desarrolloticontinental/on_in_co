	��Vu��a�4  ��              #                                � 34FC010Dutf-8 MAIN d:\newsie\on_in_co\APLIC\cbd\sunat-diario-copia.w,, PROCEDURE Texto,, PROCEDURE p-nom-aux,, PROCEDURE enable_UI,, PROCEDURE disable_UI,, PROCEDURE Carga-Temporal,, PROCEDURE adm-create-objects,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE startServerObject,, PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE restartServerObject,, PROCEDURE initializeServerObject,, PROCEDURE disconnectObject,, PROCEDURE destroyServerObject,, PROCEDURE bindServer,, PROCEDURE processAction,,INPUT pcAction CHARACTER PROCEDURE enableObject,, PROCEDURE disableObject,, PROCEDURE applyLayout,, PROCEDURE viewPage,,INPUT piPageNum INTEGER PROCEDURE viewObject,, PROCEDURE toolbar,,INPUT pcValue CHARACTER PROCEDURE selectPage,,INPUT piPageNum INTEGER PROCEDURE removePageNTarget,,INPUT phTarget HANDLE,INPUT piPage INTEGER PROCEDURE passThrough,,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER PROCEDURE notifyPage,,INPUT pcProc CHARACTER PROCEDURE initPages,,INPUT pcPageList CHARACTER PROCEDURE initializeVisualContainer,, PROCEDURE initializeObject,, PROCEDURE hidePage,,INPUT piPageNum INTEGER PROCEDURE destroyObject,, PROCEDURE deletePage,,INPUT piPageNum INTEGER PROCEDURE createObjects,, PROCEDURE constructObject,,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE PROCEDURE confirmExit,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE changePage,, PROCEDURE assignPageProperty,,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION createUiEvents,LOGICAL, FUNCTION getObjectSecured,LOGICAL, FUNCTION getObjectTranslated,LOGICAL, FUNCTION setResizeVertical,LOGICAL,INPUT plResizeVertical LOGICAL FUNCTION setResizeHorizontal,LOGICAL,INPUT plResizeHorizontal LOGICAL FUNCTION setObjectLayout,LOGICAL,INPUT pcLayout CHARACTER FUNCTION setLayoutOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setHideOnInit,LOGICAL,INPUT plHide LOGICAL FUNCTION setDisableOnInit,LOGICAL,INPUT plDisable LOGICAL FUNCTION setDefaultLayout,LOGICAL,INPUT pcDefault CHARACTER FUNCTION setAllFieldNames,LOGICAL,INPUT pcValue CHARACTER FUNCTION setAllFieldHandles,LOGICAL,INPUT pcValue CHARACTER FUNCTION getResizeVertical,LOGICAL, FUNCTION getResizeHorizontal,LOGICAL, FUNCTION getWidth,DECIMAL, FUNCTION getRow,DECIMAL, FUNCTION getObjectLayout,CHARACTER, FUNCTION getObjectEnabled,LOGICAL, FUNCTION getLayoutVariable,CHARACTER, FUNCTION getLayoutOptions,CHARACTER, FUNCTION getHideOnInit,LOGICAL, FUNCTION getHeight,DECIMAL, FUNCTION getEnabledObjHdls,CHARACTER, FUNCTION getEnabledObjFlds,CHARACTER, FUNCTION getDisableOnInit,LOGICAL, FUNCTION getDefaultLayout,CHARACTER, FUNCTION getCol,DECIMAL, FUNCTION getAllFieldNames,CHARACTER, FUNCTION getAllFieldHandles,CHARACTER, FUNCTION setStatusArea,LOGICAL,INPUT plStatusArea LOGICAL FUNCTION getObjectType,character, FUNCTION setWindowTitleViewer,LOGICAL,INPUT phViewer HANDLE FUNCTION setWaitForObject,LOGICAL,INPUT phObject HANDLE FUNCTION setUpdateTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setUpdateSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setTopOnly,LOGICAL,INPUT plTopOnly LOGICAL FUNCTION setSdoForeignFields,LOGICAL,INPUT cSdoForeignFields CHARACTER FUNCTION setSavedContainerMode,LOGICAL,INPUT cSavedContainerMode CHARACTER FUNCTION setRunMultiple,LOGICAL,INPUT plMultiple LOGICAL FUNCTION setRunDOOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setRouterTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setReEnableDataLinks,LOGICAL,INPUT cReEnableDataLinks CHARACTER FUNCTION setPrimarySdoTarget,LOGICAL,INPUT hPrimarySdoTarget HANDLE FUNCTION setPageSource,LOGICAL,INPUT phObject HANDLE FUNCTION setPageNTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setOutMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setNavigationTarget,LOGICAL,INPUT cTarget CHARACTER FUNCTION setNavigationSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setNavigationSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setMultiInstanceSupported,LOGICAL,INPUT lMultiInstanceSupported LOGICAL FUNCTION setMultiInstanceActivated,LOGICAL,INPUT lMultiInstanceActivated LOGICAL FUNCTION setInMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setFilterSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDynamicSDOProcedure,LOGICAL,INPUT pcProc CHARACTER FUNCTION setDisabledAddModeTabs,LOGICAL,INPUT cDisabledAddModeTabs CHARACTER FUNCTION setCurrentPage,LOGICAL,INPUT iPage INTEGER FUNCTION setContainerTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setContainerMode,LOGICAL,INPUT cContainerMode CHARACTER FUNCTION setCallerWindow,LOGICAL,INPUT h HANDLE FUNCTION setCallerProcedure,LOGICAL,INPUT h HANDLE FUNCTION setCallerObject,LOGICAL,INPUT h HANDLE FUNCTION pageNTargets,CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER FUNCTION getStatusArea,LOGICAL, FUNCTION getWindowTitleViewer,HANDLE, FUNCTION getWaitForObject,HANDLE, FUNCTION getUpdateTarget,CHARACTER, FUNCTION getUpdateSource,CHARACTER, FUNCTION getTopOnly,LOGICAL, FUNCTION getSdoForeignFields,CHARACTER, FUNCTION getSavedContainerMode,CHARACTER, FUNCTION getRunMultiple,LOGICAL, FUNCTION getRunDOOptions,CHARACTER, FUNCTION getReEnableDataLinks,CHARACTER, FUNCTION getPrimarySdoTarget,HANDLE, FUNCTION getPageSource,HANDLE, FUNCTION getPageNTarget,CHARACTER, FUNCTION getOutMessageTarget,HANDLE, FUNCTION getNavigationTarget,HANDLE, FUNCTION getNavigationSourceEvents,CHARACTER, FUNCTION getNavigationSource,CHARACTER, FUNCTION getMultiInstanceSupported,LOGICAL, FUNCTION getMultiInstanceActivated,LOGICAL, FUNCTION getFilterSource,HANDLE, FUNCTION getDynamicSDOProcedure,CHARACTER, FUNCTION getDisabledAddModeTabs,CHARACTER, FUNCTION getCurrentPage,INTEGER, FUNCTION getContainerTargetEvents,CHARACTER, FUNCTION getContainerTarget,CHARACTER, FUNCTION getContainerMode,CHARACTER, FUNCTION getCallerWindow,HANDLE, FUNCTION getCallerProcedure,HANDLE, FUNCTION enablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION disablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION widgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION widgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION widgetIsTrue,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsModified,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetIsFocused,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsBlank,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetLongcharValue,LONGCHAR,INPUT pcName CHARACTER FUNCTION widgetHandle,HANDLE,INPUT pcName CHARACTER FUNCTION viewWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION toggleWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION resetWidgetValue,LOGICAL,INPUT pcNameList CHARACTER FUNCTION highlightWidget,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER FUNCTION hideWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION formattedWidgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION formattedWidgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION enableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION enableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION disableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION disableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION clearWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION blankWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION assignWidgetValueList,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION assignWidgetValue,LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER FUNCTION assignFocusedWidget,LOGICAL,INPUT pcName CHARACTER       �5              �              �� �5  \�              �t              �)    +   �n �  7   \s `  8   �v �   >   �w d"  ?   � 8  @   L� @  A   �� �  B   � D
  C           \� �  ? � �"  iSO8859-1                                                                           �4   - �                  5                  �                  ��                    �1     2   k    \�  5         �  �   p5      |5          �                                             PROGRESS                         L	           
    
                    �              �                                                                                                     
               INTEGRAL                         PROGRESS                         l     �  �      �   C                      �ɺ[            �  Í                              �  l                      �  |  (     CODCTANOMCTAAFTDCBPIDAUXPIDDOCCIERESCODMONTPOCMBACTIVOCLFAUXPIDREFNROCHQCODDOCCODBCONROCTAAN1CTACC1CTASECTORCODDIVTPOGTOCODCIAPIDCCOVCODCIACHQFINCHQINICTAANTCODOPETMMOVCTASUSTENTOLIBRE_C01LIBRE_C02LIBRE_C03LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02LIBRE_F03LIBRE_L01LIBRE_L02                                                                         	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          '          (          )          �             �                                                                                          �             d             P                                                                                          R             �             �                                                                                          [             \             H                                                                                          e             �             �                                                                                          n  ��         T  	           @                                                                                          w             �  
           �                                                                                          �             |	             8	                                                                                          �             �	        t	  
    
                  `	  (
             �	                                                                                                    
  �
  *       
  
    
                  
  �
             �
                                                                                          *          
  P  <      �
  
    
                  �
  �             <                                                                                          <          
  �  I      x  
    
                  d  ,             �                                                                                          I          
  �  \      $  
    
                    �             �                                                                                          \          
  T  n      �  
    
                  �  �             @                                                                                          n          
     �      |  
    
                  h  0             �                                                                                          �          
  �  �      (  
    
                    �             �                                                                                          �          
  X  �      �                         �  �             D                                                                                          �              �      �                        l  4             �                                                                                          �            �  �      ,  
    
                    �             �                                                                                          �          
  \  �      �  
    
                  �  �             H                                                                                          �          
    �      �  
    
                  p  8             �                                                                                          �          
  �  �      0                          �             �                                                                                          �            `  �      �                        �  �             L                                                                                          �                    �                        t  <             �                                                                                                                4                           �             �                                                                                                        !      �                                �ɺ[               ��  ?                           �  8                      �  H  F 
     TASK-NOLLAVE-ILLAVE-CCAMPO-DCAMPO-FCAMPO-ICAMPO-CLLAVE-FLLAVE-DCAMPO-L                                                                    	          
                   �  "      �                                �ɺ[               ��  ?                           �  8                      h  #   '   �      '                          �ɺ[            /   ��                              �                        L    2-     CODCIACODDIVPERIODONROMESCODOPENROASTNROVOUCODMONTPOCMBFCHASTNOTASTGLOASTTOTITMUSUARIODBEMN1HBEMN1DBEMN2HBEMN2DBEMN3HBEMN3FLGESTCTACJAGIRADONROCHQIMPCHQFCHMODNROTRACODDOCCODAUXFLGTRAC-FCAJAPORDETPORRETNRORETLIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02LIBRE_L01LIBRE_L02                                                                        	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          '          (          )          *          +          ,          -          .          �  $   g   �      g                          �ɺ[            o   �	                              �  �                      P  �  U      CODOPENOMOPESIGLASORIGENUSUARIOSLIBRORESUMECODMONTPOCMBCODCIACORMESCODANTCODCTATIPMOV                                                                         	          
                                                            �   %   �   �      �                          �ɺ[            �   9	                              �  x                      �  �  b;     CODCIACODDIVPERIODONROMESCODOPENROASTCODMONNROITMTPOITMCODCTACLFAUXCODAUXNRORUCCODDOCNRODOCCODREFNROREFFCHDOCFCHVTOGLODOCTPOMOVTPOCMBIMPMN1IMPMN2IMPMN3RELACIONFLGACTTMCCOCTAAUTCTRCTAC-FCAJAREL-AC-CBDISCCOORDCMPCNDCMPCODBCOCHR_01CHR_02CHR_03CHR_04CHR_05CHR_06CHR_07CHR_08CHR_09CHR_10DEC_01DEC_02DEC_03DEC_04DEC_05DTE_01DTE_02DTE_03DTE_04DTE_05LOG_01LOG_02                                                                        	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2          3          4          5          6          7          8          9          :          ;          <          0!  &   �   �      �                          �ɺ[            �   9	                             �  x                      �!  '      �                               �ɺ[               ��                              �  8                      �(  (   !  �      !                         �\            	!  '�                              �  0"                      �$  @"  RT     CODCLINOMCLIDIRCLIRUCTPOCLILOCCLIFAXCLIFCHINGFLGSITCODPOSCODCIAREFERENCIASCODPAISCODDEPTCODPROVCODDISTGIRCLICNDVTATELFNOSE-MAILFCHACTCLFCLIUSUARIOREPLEGFNREPRTRANSPORTEMONLCIMPLCCODVENDIRENTFCHCESCONTACFCHVLCCODANTUSRLCVENANTFLAGAUTUSRAUTOBSAUTCODDIVDEPENTPROENTDISENTLINCREUSRCRETOTUSOFCHAUTAVAL1AVAL2TLFLOGJFELOGDNIRUCOLDCANALCLFCOMNROCARDDIRREFCODUNICOAPEPATAPEMATNOMBRECOB_DIASCOB_HORARIOCOB_DIRECCIONCOB_CARTACOB_GLOSACODIBCCLFCLI2LIBRE_C01LIBRE_C02LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02LIBRE_L01LIBRE_L02FLAGTIPOFLAGUSUARIOFLAGMIGRACIONFLAGFECHACM_CLFCLI_PCM_CLFCLI_TSWBAJASUNATSWCARGASUNAT                                                                        	          
                                                                                                                                                                                                                                    !          "          #          $          %          &          '          (          )          *          +          ,          -         .         /          0         1         2         3          4         5          6          7          8          9          :          ;          <          =          >          ?          @          A          B          C          D          E          F          G          H          I          J          K          L          M          N          O          Q          R          S          T          U          V          x,  )   !!  �      !!                         �M�]            )!  ��                              �  )                      \*  )  E-     REFERENCIASCODPRONOMPRODIRPRORUCCODCIATPOPROLOCPROFAXPROFCHINGFLGSITCODPOSCODPAISCODDEPTCODPROVCODDISTGIRPROCNDCMPCONTACTOSTELFNOSE-MAILFCHACTCLFPROUSUARIORUCOLDAPEPATAPEMATNOMBREPERSONATPOENTPRIORIDADPAGOREPLEGALREPLEGALCARGOLIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02STKMINSTKMAXSTKREP                                                                         	          
                                                                                                                                                                                                                                     !          "          #          $          %          &          '          (          )          *          +          ,          -          .          �/  *   �  �      �                         �ɺ[            D!  Í                              �  �,                      .  -  (     CODCTANOMCTAAFTDCBPIDAUXPIDDOCCIERESCODMONTPOCMBACTIVOCLFAUXPIDREFNROCHQCODDOCCODBCONROCTAAN1CTACC1CTASECTORCODDIVTPOGTOCODCIAPIDCCOVCODCIACHQFINCHQINICTAANTCODOPETMMOVCTASUSTENTOLIBRE_C01LIBRE_C02LIBRE_C03LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02LIBRE_F03LIBRE_L01LIBRE_L02                                                                         	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          '          (          )              +   ]!  �      ]!                         �ɺ[            e!  �;                              �  x0                      1  �0  �      CLFAUXCODAUXNOMAUXCODCIAVCODCIALIBRE_C01LIBRE_C02LIBRE_C03LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02LIBRE_F03LIBRE_L01LIBRE_L02LIBRE_L03LIBRE_D03                                                                       	          
                                                                                                        ��                                              ! �          �3  @4  h �H2                                                                                                                                                                                  
             
             
                                         
                                                                                                                h   x   �   �   �   �   �   �   �   �       (  8  H  X  h  x  �  �  �  �  �  �  �      h   x   �   �   �   �   �   �   �   �      (  8  H  X  h  x  �  �  �  �  �  �  �    ��                                               *          ����                            �      z"  ! lf    �"  # ��    �"  $ ښ    �"  % m$    �"  ( �    �"  ) ��    �"  * �m    �"  + �    undefined                                                               �       0�  �   l   @�    P�                  �����               ,�                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D       p
     9          assignFocusedWidget         �      �            LOGICAL,INPUT pcName CHARACTER  assignWidgetValue   �      �      $    $       LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER  assignWidgetValueList         \      �    6       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER  blankWidget t      �          L       LOGICAL,INPUT pcNameList CHARACTER  clearWidget �      @      l    X       LOGICAL,INPUT pcNameList CHARACTER  disableRadioButton  L      �      �    d       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    disableWidget   �            4    w       LOGICAL,INPUT pcNameList CHARACTER  enableRadioButton         X      �    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    enableWidget    l      �      �    �       LOGICAL,INPUT pcNameList CHARACTER  formattedWidgetValue    �             X  	  �       CHARACTER,INPUT pcName CHARACTER    formattedWidgetValueList    8      |      �  
  �       CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    hideWidget  �      �      (   
 �       LOGICAL,INPUT pcNameList CHARACTER  highlightWidget       L      |    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER  resetWidgetValue    \      �      �    �       LOGICAL,INPUT pcNameList CHARACTER  toggleWidget    �            H    �       LOGICAL,INPUT pcNameList CHARACTER  viewWidget  (      l      �   
       LOGICAL,INPUT pcNameList CHARACTER  widgetHandle    x      �      �          HANDLE,INPUT pcName CHARACTER   widgetLongcharValue �            @    #      LONGCHAR,INPUT pcName CHARACTER widgetIsBlank          `      �    7      LOGICAL,INPUT pcNameList CHARACTER  widgetIsFocused p      �      �    E      LOGICAL,INPUT pcName CHARACTER  widgetIsModified    �      	      8	    U      LOGICAL,INPUT pcNameList CHARACTER  widgetIsTrue    	      \	      �	    f      LOGICAL,INPUT pcName CHARACTER  widgetValue l	      �	      �	    s      CHARACTER,INPUT pcName CHARACTER    widgetValueList �	      �	      ,
          CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    ��    �  �
        �       4   �����                                       ��                  �  �                  `�                       �  �
  �    �  4  D      �       4   �����       $  �  p  ���                       �   @         �               � ߱              �  �  �            4   ����      $  �  �  ���                       \  @         H              � ߱        assignPageProperty                              �  �      ��                  *  -  �              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                �               ��                             ��                            ����                            changePage                                �      ��                  /  0                 ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmExit                               �      ��                  2  4                 d��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  8           ��                            ����                            constructObject                             4        ��                  6  ;  L              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             d               �� 
  �             �  
             ��   �             �               �� 
                 �  
         ��                            ����                            createObjects                               �  �      ��                  =  >  �              آ�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deletePage                              �  �      ��                  @  B  �              P��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                             ��                            ����                            destroyObject                                 �      ��                  D  E                �(�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hidePage                                  �      ��                  G  I                `�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  4           ��                            ����                            initializeObject                                4        ��                  K  L  L              �|�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeVisualContainer                               D  ,      ��                  N  O  \              L}�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initPages                               D  ,      ��                  Q  S  \              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  t           ��                            ����                            notifyPage                              l  T      ��                  U  W  �              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            passThrough                             �  |      ��                  Y  \  �              <��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                  �           ��                            ����                            removePageNTarget                               �  �      ��                  ^  a                ���                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  P               
             ��                  D           ��                            ����                            selectPage                              <  $      ��                  c  e  T              t�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  l           ��                            ����                            toolbar                             `  H      ��                  g  i  x              ı�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            viewObject                              �  p      ��                  k  l  �              �u�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewPage                                �   p       ��                  n  p  �               0��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �            ��                            ����                            disablePagesInFolder    
       !      X!    r      LOGICAL,INPUT pcPageInformation CHARACTER   enablePagesInFolder 8!      �!      �!    �      LOGICAL,INPUT pcPageInformation CHARACTER   getCallerProcedure  �!      �!      "    �      HANDLE, getCallerWindow �!       "      P"    �      HANDLE, getContainerMode    0"      X"      �"    �      CHARACTER,  getContainerTarget  l"      �"      �"    �      CHARACTER,  getContainerTargetEvents    �"      �"      #    �      CHARACTER,  getCurrentPage  �"       #      P#    �      INTEGER,    getDisabledAddModeTabs  0#      \#      �#     
      CHARACTER,  getDynamicSDOProcedure  t#      �#      �#  !  !      CHARACTER,  getFilterSource �#      �#      $  "  8      HANDLE, getMultiInstanceActivated   �#      $      X$  #  H      LOGICAL,    getMultiInstanceSupported   8$      d$      �$  $  b      LOGICAL,    getNavigationSource �$      �$      �$  %  |      CHARACTER,  getNavigationSourceEvents   �$      �$      (%  &  �      CHARACTER,  getNavigationTarget %      4%      h%  '  �      HANDLE, getOutMessageTarget H%      p%      �%  (  �      HANDLE, getPageNTarget  �%      �%      �%  )  �      CHARACTER,  getPageSource   �%      �%      &  *  �      HANDLE, getPrimarySdoTarget �%       &      T&  +  �      HANDLE, getReEnableDataLinks    4&      \&      �&  ,        CHARACTER,  getRunDOOptions t&      �&      �&  -        CHARACTER,  getRunMultiple  �&      �&      '  .  (      LOGICAL,    getSavedContainerMode   �&      '      P'  /  7      CHARACTER,  getSdoForeignFields 0'      \'      �'  0  M      CHARACTER,  getTopOnly  p'      �'      �'  1 
 a      LOGICAL,    getUpdateSource �'      �'      (  2  l      CHARACTER,  getUpdateTarget �'      (      @(  3  |      CHARACTER,  getWaitForObject     (      L(      �(  4  �      HANDLE, getWindowTitleViewer    `(      �(      �(  5  �      HANDLE, getStatusArea   �(      �(      �(  6  �      LOGICAL,    pageNTargets    �(      )      4)  7  �      CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER setCallerObject )      l)      �)  8  �      LOGICAL,INPUT h HANDLE  setCallerProcedure  |)      �)      �)  9  �      LOGICAL,INPUT h HANDLE  setCallerWindow �)       *      0*  :  �      LOGICAL,INPUT h HANDLE  setContainerMode    *      H*      |*  ;         LOGICAL,INPUT cContainerMode CHARACTER  setContainerTarget  \*      �*      �*  <        LOGICAL,INPUT pcObject CHARACTER    setCurrentPage  �*      �*      ,+  =  $      LOGICAL,INPUT iPage INTEGER setDisabledAddModeTabs  +      H+      �+  >  3      LOGICAL,INPUT cDisabledAddModeTabs CHARACTER    setDynamicSDOProcedure  `+      �+      �+  ?  J      LOGICAL,INPUT pcProc CHARACTER  setFilterSource �+      ,      8,  @  a      LOGICAL,INPUT phObject HANDLE   setInMessageTarget  ,      X,      �,  A  q      LOGICAL,INPUT phObject HANDLE   setMultiInstanceActivated   l,      �,      �,  B  �      LOGICAL,INPUT lMultiInstanceActivated LOGICAL   setMultiInstanceSupported   �,      -      T-  C  �      LOGICAL,INPUT lMultiInstanceSupported LOGICAL   setNavigationSource 4-      �-      �-  D  �      LOGICAL,INPUT pcSource CHARACTER    setNavigationSourceEvents   �-      �-      .  E  �      LOGICAL,INPUT pcEvents CHARACTER    setNavigationTarget �-      <.      p.  F  �      LOGICAL,INPUT cTarget CHARACTER setOutMessageTarget P.      �.      �.  G  �      LOGICAL,INPUT phObject HANDLE   setPageNTarget  �.      �.      /  H        LOGICAL,INPUT pcObject CHARACTER    setPageSource   �.      8/      h/  I        LOGICAL,INPUT phObject HANDLE   setPrimarySdoTarget H/      �/      �/  J  +      LOGICAL,INPUT hPrimarySdoTarget HANDLE  setReEnableDataLinks    �/      �/      0  K  ?      LOGICAL,INPUT cReEnableDataLinks CHARACTER  setRouterTarget �/      H0      x0  L  T      LOGICAL,INPUT phObject HANDLE   setRunDOOptions X0      �0      �0  M  d      LOGICAL,INPUT pcOptions CHARACTER   setRunMultiple  �0      �0      1  N  t      LOGICAL,INPUT plMultiple LOGICAL    setSavedContainerMode   �0      @1      x1  O  �      LOGICAL,INPUT cSavedContainerMode CHARACTER setSdoForeignFields X1      �1      �1  P  �      LOGICAL,INPUT cSdoForeignFields CHARACTER   setTopOnly  �1      2      02  Q 
 �      LOGICAL,INPUT plTopOnly LOGICAL setUpdateSource 2      P2      �2  R  �      LOGICAL,INPUT pcSource CHARACTER    setUpdateTarget `2      �2      �2  S  �      LOGICAL,INPUT pcTarget CHARACTER    setWaitForObject    �2      �2      ,3  T  �      LOGICAL,INPUT phObject HANDLE   setWindowTitleViewer    3      L3      �3  U  �      LOGICAL,INPUT phViewer HANDLE   getObjectType   d3      �3      �3  V  �      CHARACTER,  setStatusArea   �3      �3      4  W        LOGICAL,INPUT plStatusArea LOGICAL  applyLayout                             �4  �4      ��                  �  �  �4              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableObject                               �5  �5      ��                  �  �  �5              �/�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            enableObject                                �6  �6      ��                  �  �  �6              �/�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �7  �7      ��                  �  �  �7              �0�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            processAction                               �8  �8      ��                  �  �  �8              ,��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  9           ��                            ����                            getAllFieldHandles  �3      p9      �9  X        CHARACTER,  getAllFieldNames    �9      �9      �9  Y  -      CHARACTER,  getCol  �9      �9      :  Z  >      DECIMAL,    getDefaultLayout    �9      $:      X:  [  E      CHARACTER,  getDisableOnInit    8:      d:      �:  \  V      LOGICAL,    getEnabledObjFlds   x:      �:      �:  ]  g      CHARACTER,  getEnabledObjHdls   �:      �:      ;  ^  y      CHARACTER,  getHeight   �:      $;      P;  _ 	 �      DECIMAL,    getHideOnInit   0;      \;      �;  `  �      LOGICAL,    getLayoutOptions    l;      �;      �;  a  �      CHARACTER,  getLayoutVariable   �;      �;      <  b  �      CHARACTER,  getObjectEnabled    �;      <      L<  c  �      LOGICAL,    getObjectLayout ,<      X<      �<  d  �      CHARACTER,  getRow  h<      �<      �<  e  �      DECIMAL,    getWidth    �<      �<      �<  f  �      DECIMAL,    getResizeHorizontal �<       =      4=  g  �      LOGICAL,    getResizeVertical   =      @=      t=  h  	      LOGICAL,    setAllFieldHandles  T=      �=      �=  i  	      LOGICAL,INPUT pcValue CHARACTER setAllFieldNames    �=      �=      >  j  0	      LOGICAL,INPUT pcValue CHARACTER setDefaultLayout    �=      (>      \>  k  A	      LOGICAL,INPUT pcDefault CHARACTER   setDisableOnInit    <>      �>      �>  l  R	      LOGICAL,INPUT plDisable LOGICAL setHideOnInit   �>      �>      ?  m  c	      LOGICAL,INPUT plHide LOGICAL    setLayoutOptions    �>      $?      X?  n  q	      LOGICAL,INPUT pcOptions CHARACTER   setObjectLayout 8?      |?      �?  o  �	      LOGICAL,INPUT pcLayout CHARACTER    setResizeHorizontal �?      �?      @  p  �	      LOGICAL,INPUT plResizeHorizontal LOGICAL    setResizeVertical   �?      0@      d@  q  �	      LOGICAL,INPUT plResizeVertical LOGICAL  getObjectTranslated D@      �@      �@  r  �	      LOGICAL,    getObjectSecured    �@      �@       A  s  �	      LOGICAL,    createUiEvents  �@      A      <A  t  �	      LOGICAL,    bindServer                              �A  �A      ��                  �  �  �A              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �B  �B      ��                  �  �  �B              (��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �C  �C      ��                  �  �  �C              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �D  �D      ��                  �  �  E              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �E  �E      ��                  �  �  F              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                              G  �F      ��                  �  �  G              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             H  �G      ��                  �  �  H              �[�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 4H  
         ��                            ����                            startServerObject                               4I  I      ��                  �  �  LI              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                8J   J      ��                  �  �  PJ              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  hJ           ��                            ����                            getAppService   A      �J       K  u  �	      CHARACTER,  getASBound  �J      K      8K  v 
 �	      LOGICAL,    getAsDivision   K      DK      tK  w  
      CHARACTER,  getASHandle TK      �K      �K  x  
      HANDLE, getASHasStarted �K      �K      �K  y  
      LOGICAL,    getASInfo   �K      �K      L  z 	 /
      CHARACTER,  getASInitializeOnRun    �K      (L      `L  {  9
      LOGICAL,    getASUsePrompt  @L      lL      �L  |  N
      LOGICAL,    getServerFileName   |L      �L      �L  }  ]
      CHARACTER,  getServerOperatingMode  �L      �L       M  ~  o
      CHARACTER,  runServerProcedure   M      ,M      `M    �
      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   @M      �M      �M  �  �
      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   �M      �M      ,N  �  �
      LOGICAL,INPUT pcDivision CHARACTER  setASHandle N      PN      |N  �  �
      LOGICAL,INPUT phASHandle HANDLE setASInfo   \N      �N      �N  � 	 �
      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    �N      �N       O  �  �
      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt   O      DO      tO  �  �
      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   TO      �O      �O  �  �
      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  �O      �O      $P  �        LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �P  �P      ��                  �  �  �P              �'�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  DQ             Q  
             ��   lQ             8Q               �� 
                 `Q  
         ��                            ����                            addMessage                              XR  @R      ��                  �  �  pR              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �R             �R               ��   �R             �R               ��                  �R           ��                            ����                            adjustTabOrder                              �S  �S      ��                  �  �  �S              p�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  8T             T  
             �� 
  `T             ,T  
             ��                  TT           ��                            ����                            applyEntry                              LU  4U      ��                  �  �  dU              �j�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  |U           ��                            ����                            changeCursor                                xV  `V      ��                  �  �  �V              �F�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �V           ��                            ����                            createControls                              �W  �W      ��                  �  �  �W              �g�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �X  �X      ��                  �  �  �X              \j�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �Y  �Y      ��                  �  �  �Y              `k�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �Z  �Z      ��                  �  �  �Z              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �[  �[      ��                  �  �  �[              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �\  �\      ��                  �  �  �\              �K�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �]  �]      ��                  �  �  �]              HL�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              �^  �^      ��                  �  �  �^              ��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  ,_             �^  
             ��   T_              _               ��   |_             H_               ��                  p_           ��                            ����                            modifyUserLinks                             l`  T`      ��                  �  �  �`              �$�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �`             �`               ��   �`             �`               �� 
                 �`  
         ��                            ����                            removeAllLinks                              �a  �a      ��                  �  �   b              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              �b  �b      ��                  �  �   c              \��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  Lc             c  
             ��   tc             @c               �� 
                 hc  
         ��                            ����                            repositionObject                                hd  Pd      ��                  �    �d              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �d             �d               ��                  �d           ��                            ����                            returnFocus                             �e  �e      ��                      �e              l��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �e  
         ��                            ����                            showMessageProcedure                                �f  �f      ��                      g              X��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   Pg             g               ��                  Dg           ��                            ����                            toggleData                              <h  $h      ��                      Th              �N�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  lh           ��                            ����                            viewObject                              di  Li      ��                      |i              $v�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  P      �i       j  � 
 f      LOGICAL,    assignLinkProperty  �i      j      @j  �  q      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages    j      �j      �j  �  �      CHARACTER,  getChildDataKey �j      �j      k  �  �      CHARACTER,  getContainerHandle  �j      k      Dk  �  �      HANDLE, getContainerHidden  $k      Lk      �k  �  �      LOGICAL,    getContainerSource  `k      �k      �k  �  �      HANDLE, getContainerSourceEvents    �k      �k      l  �  �      CHARACTER,  getContainerType    �k      l      Dl  �  �      CHARACTER,  getDataLinksEnabled $l      Pl      �l  �        LOGICAL,    getDataSource   dl      �l      �l  �        HANDLE, getDataSourceEvents �l      �l      �l  �  '      CHARACTER,  getDataSourceNames  �l      m      <m  �  ;      CHARACTER,  getDataTarget   m      Hm      xm  �  N      CHARACTER,  getDataTargetEvents Xm      �m      �m  �  \      CHARACTER,  getDBAware  �m      �m      �m  � 
 p      LOGICAL,    getDesignDataObject �m      �m      0n  �  {      CHARACTER,  getDynamicObject    n      <n      pn  �  �      LOGICAL,    getInstanceProperties   Pn      |n      �n  �  �      CHARACTER,  getLogicalObjectName    �n      �n      �n  �  �      CHARACTER,  getLogicalVersion   �n      o      8o  �  �      CHARACTER,  getObjectHidden o      Do      to  �  �      LOGICAL,    getObjectInitialized    To      �o      �o  �  �      LOGICAL,    getObjectName   �o      �o      �o  �        CHARACTER,  getObjectPage   �o       p      0p  �        INTEGER,    getObjectParent p      <p      lp  �        HANDLE, getObjectVersion    Lp      tp      �p  �  .      CHARACTER,  getObjectVersionNumber  �p      �p      �p  �  ?      CHARACTER,  getParentDataKey    �p      �p      ,q  �  V      CHARACTER,  getPassThroughLinks q      8q      lq  �  g      CHARACTER,  getPhysicalObjectName   Lq      xq      �q  �  {      CHARACTER,  getPhysicalVersion  �q      �q      �q  �  �      CHARACTER,  getPropertyDialog   �q      �q      0r  �  �      CHARACTER,  getQueryObject  r      <r      lr  �  �      LOGICAL,    getRunAttribute Lr      xr      �r  �  �      CHARACTER,  getSupportedLinks   �r      �r      �r  �  �      CHARACTER,  getTranslatableProperties   �r      �r      0s  �  �      CHARACTER,  getUIBMode  s      <s      hs  � 
       CHARACTER,  getUserProperty Hs      ts      �s  �        CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    �s      �s      t  �        CHARACTER,INPUT pcPropList CHARACTER    linkHandles �s      ,t      Xt  �  1      CHARACTER,INPUT pcLink CHARACTER    linkProperty    8t      |t      �t  �  =      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry �t      �t      u  �  J      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   �t      �u      �u  �  V      CHARACTER,INPUT piMessage INTEGER   propertyType    �u      �u      v  �  d      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  �u      ,v      \v  �  q      CHARACTER,  setChildDataKey <v      hv      �v  �  �      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  xv      �v      �v  �  �      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  �v      w      Hw  �  �      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    (w      hw      �w  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled �w      �w      �w  �  �      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   �w      $x      Tx  �  �      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents 4x      tx      �x  �  �      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  �x      �x      y  �        LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   �x      ,y      \y  �        LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents <y      �y      �y  �  &      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  �y      �y      z  � 
 :      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject �y      $z      Xz  �  E      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    8z      �z      �z  �  Y      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   �z      �z      {  �  j      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    �z      ,{      d{  �  �      LOGICAL,INPUT c CHARACTER   setLogicalVersion   D{      �{      �{  �  �      LOGICAL,INPUT cVersion CHARACTER    setObjectName   �{      �{      |  �  �      LOGICAL,INPUT pcName CHARACTER  setObjectParent �{      (|      X|  �  �      LOGICAL,INPUT phParent HANDLE   setObjectVersion    8|      x|      �|  �  �      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    �|      �|      }  �  �      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks �|      0}      d}  �  �      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   D}      �}      �}  �  �      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  �}      �}      ~  �        LOGICAL,INPUT cVersion CHARACTER    setRunAttribute �}      4~      d~  �  $      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   D~      �~      �~  �  4      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   �~      �~         �  F      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode         D      p  � 
 `      LOGICAL,INPUT pcMode CHARACTER  setUserProperty P      �      �  �  k      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage �       �      ,�  �  {      LOGICAL,INPUT pcMessage CHARACTER   Signature   �      P�      |�  � 	 �      CHARACTER,INPUT pcName CHARACTER    t�    (  ��  8�      �      4   �����                H�                      ��                  )  V                  $�1                       )  ̀        *  d�  ��      �      4   �����                ��                      ��                  +  U                  ��1                       +  t�  ��    B  �  ��      �      4   �����                ��                      ��                  N  P                  D�0                       N  �         O                                  T     
                    � ߱        �  $  R  Ă  ���                           $  T  H�  ���                       �                         � ߱        ��    Z  ��  �      �      4   �����                �                      ��                  [  	                  ��0                       [  ��  P�  o   ^      ,                                 ��  $   _  |�  ���                       $  @                       � ߱        ��  �   `  D      Є  �   a  �      �  �   c  ,      ��  �   e  �      �  �   g         �  �   i  �      4�  �   j        H�  �   k  @      \�  �   n  �      p�  �   p  (      ��  �   q  �      ��  �   s         ��  �   t  �      ��  �   u  �      ԅ  �   v  T	      �  �   w  �	      ��  �   }  
      �  �     x
      $�  �   �  �
      8�  �   �  (      L�  �   �  �      `�  �   �        t�  �   �  �      ��  �   �        ��  �   �  �      ��  �   �  �      Ć  �   �  l      ؆  �   �  �      �  �   �         �  �   �  X      �  �   �  �      (�  �   �        <�  �   �  D      P�  �   �  �      d�  �   �  �      x�  �   �  8      ��  �   �  t      ��  �   �  �      ��  �   �  �      ȇ  �   �  (      ܇  �   �  d      ��  �   �  �      �  �   �  �      �  �   �            �   �  T                      D�          ��  ��      ��                  F	  t	  Ȉ              |�0                    O   ����    e�          O   ����    R�          O   ����    ��      �     
                @                     P                         � ߱        p�  $ Z	  ��  ���                           O   r	  ��  ��  �               ܉          ̉  ԉ    ��                                             ��                            ����                                �3      ,�      ��     6     �                      V ��  �                     @�    �	  ��  �      �      4   �����                (�                      ��                  �	  
                  �42                       �	  ��  <�  �   �	  �      P�  �   �	  p      d�  �   �	  �      x�  �   �	  h      ��  �   �	  �      ��  �   �	  `      ��  �   �	  �      ȋ  �   �	  P      ܋  �   �	  �      ��  �   �	  H      �  �   �	  �      �  �   �	  8      ,�  �   �	  �          �   �	  0      �    &
  \�  ،      �      4   �����                �                      ��                  '
  �
                  t62                       '
  l�  ��  �   )
         �  �   *
  t      $�  �   +
  �      8�  �   ,
  d      L�  �   -
  �      `�  �   .
  L      t�  �   /
  �      ��  �   0
  <       ��  �   1
  �       ��  �   2
  $!      č  �   3
  �!      ؍  �   4
  "      �  �   5
  �"       �  �   6
  #      �  �   7
  �#      (�  �   8
  �#      <�  �   9
  x$      P�  �   :
  �$      d�  �   ;
  p%      x�  �   <
  �%      ��  �   =
  h&      ��  �   >
  �&      ��  �   ?
  `'      Ȏ  �   @
  �'      ܎  �   A
  X(      ��  �   B
  �(      �  �   C
  P)          �   D
  �)      4�    �
  4�  ��      4*      4   ����4*                ��                      ��                  �
  s                  $��                       �
  D�  ԏ  �   �
  �*      �  �   �
  +      ��  �   �
  �+      �  �   �
   ,      $�  �   �
  t,      8�  �   �
  �,      L�  �   �
  \-      `�  �   �
  �-      t�  �   �
  .      ��  �   �
  H.      ��  �   �
  �.      ��  �   �
  �.      Đ  �   �
  l/      ؐ  �   �
  �/      �  �   �
  \0       �  �   �
  �0      �  �   �
  D1      (�  �   �
  �1      <�  �   �
  <2      P�  �   �
  x2      d�  �   �
  �2      x�  �   �
  `3      ��  �   �
  �3      ��  �   �
  4      ��  �   �
  L4      ȑ  �   �
  �4      ܑ  �   �
  5      �  �   �
  @5      �  �   �
  |5      �  �   �
  �5      ,�  �   �
  �5      @�  �   �
  06      T�  �   �
  l6      h�  �   �
  �6      |�  �   �
  7      ��  �   �
  X7      ��  �   �
  �7      ��  �   �
  �7      ̒  �   �
  8      ��  �   �
  H8      ��  �   �
  �8      �  �   �
  �8      �  �   �
  l9      0�  �   �
  �9      D�  �   �
  T:      X�  �   �
  �:      l�  �   �
  L;      ��  �   �
  �;      ��  �   �
  D<      ��  �   �
  �<      ��  �   �
  <=      Г  �   �
  x=      �  �   �
  �=      ��  �   �
  0>      �  �      l>       �  �     �>          �     ?      ��  $    `�  ���                       �?     
                    � ߱        $�    �  ��  ��      �?      4   �����?      /   �  �     ��                          3   �����?            �                      3   �����?  x�    �  @�  ��  ��  �?      4   �����?  	              ̕                      ��             	     �  G                  P�1                       �  P�  ��  �   �  D@      8�  $  �  �  ���                       p@     
                    � ߱        L�  �   �  �@      ��  $   �  x�  ���                       �@  @         �@              � ߱        `�  $  �  Ж  ���                       A                         � ߱        �A     
                �A                     LC  @        
 C              � ߱        �  V   �  ��  ���                        XC                     �C                     �C                         � ߱        ��  $  �  ��  ���                       �D     
                E                     TF  @        
 F              � ߱        �  V     �  ���                        `F     
                �F                     ,H  @        
 �G              � ߱            V   +  ��  ���                        
              p�                      ��             
     I  �                  ��1                       I  <�  @H     
                �H                     J  @        
 �I          pJ  @        
 0J          �J  @        
 �J          4K  @        
 �J              � ߱            V   ^  ��  ���                        adm-clone-props $�  ��              �     7     `                          \  �                     start-super-proc    ��  �  �           �     8                                  �                     �    �  ��  ��      �N      4   �����N      /   �  Л     ��                          3   �����N             �                      3   �����N  h�  $    <�  ���                       O                         � ߱        $�    )  ��   �  ��  ,O      4   ����,O                t�                      ��                  *  .                  �R2                       *  ��  @O                     TO                     hO                         � ߱            $  +  �  ���                             /  ��  ��      �O      4   �����O  �O                         � ߱            $  0  ̝  ���                        �    7  @�  P�  ��  �O      4   �����O      $  8  |�  ���                       �O                         � ߱            �   U  �O      (P     
                �P                     �Q  @        
 �Q              � ߱        L�  V   i  ��  ���                        `�  �   �   R      ��      |�  ��      @R      4   ����@R      /     ��     ȟ                          3   ����PR            �                      3   ����pR  ��  $  #  $�  ���                       �R                         � ߱        �R     
                4S                     �T  @        
 DT              � ߱        �  V   -  P�  ���                        ��    �  ��  x�      �T      4   �����T                ��                      ��                  �  �                  d�0                       �  �      g   �  ��         I�d�                           h�          8�   �      ��                  �      P�              ��0                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  ��     ��  �T                      3   �����T  Ԣ     
   Ģ                      3   �����T         
   ��                      3   �����T    ��                              ��        *                  ����                                        ��              9      �                      g                               ȥ  g   �  أ          I�	l�                           ��          p�  X�      ��                  �  �  ��              0��                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  ̤     ܤ  �T                      3   �����T            ��                      3   �����T    ��                              ��        *                  ����                                        �              :      �                      g                               Ч  g   �  �          I�	t�                           ��          x�  `�      ��                  �  �  ��              ���                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  Ԧ     �  0U                      3   ����U            �                      3   ����8U    ��                              ��        *                  ����                                        ��              ;      �                      g                               0�    �  �  h�      TU      4   ����TU                x�                      ��                  �  �                  �n2                       �  ��  �  /   �  ��     ��                          3   ����dU            Ԩ                      3   �����U  �  /  �  �      �  �U                      3   �����U  P�     
   @�                      3   �����U  ��        p�                      3   �����U  ��        ��                      3   �����U            Щ                      3   ����V  �    �  ��  �      ,V      4   ����,V      /  �  8�     H�  �V                      3   �����V  x�     
   h�                      3   �����V  ��        ��                      3   �����V  ت        Ȫ                      3   �����V            ��                      3   �����V        �  $�  4�      W      4   ����W      /  �  `�     p�  pW                      3   ����PW  ��     
   ��                      3   ����xW  Ы        ��                      3   �����W   �        �                      3   �����W             �                      3   �����W  Ȭ     �  �W                                     �W     
                dX                     �Y  @        
 tY              � ߱        X�  V   e  d�  ���                        �Y     
                DZ                     �[  @        
 T[              � ߱        ̭  V   �  ��  ���                        �[  @         �[          �[  @         �[              � ߱        ��  $   �  ��  ���                       ��  g   �  �         I6P�                            خ          ��  ��      ��                  �  �  ��              �0                    O   ����    e�          O   ����    R�          O   ����    ��            �  �[  }        ��                              ��        *                  ����                                        $�              <      �                      g                               �  g   �  į         I"��                           �          \�  D�      ��                  �  �  t�              ��0                    O   ����    e�          O   ����    R�          O   ����    ��                                     
       
                                 � ߱        �  $   �  ��   �                           /   �  H�                                 3   ����\    ��                              ��        *                  ����                                        د              =      X�                      g                               d�      0�  ��      $\      4   ����$\                ��                      ��                                      X�                         @�   �  	    �                                        3   ����8\  <�  /   	  ,�                                 3   �����\  L�  �   
  �\      O     ��  ��  �\  �      ��  ��      �\      4   �����\      $     ��  ���                       8]  @         $]              � ߱        ��  /     �                                 3   ����@]                д          ��  ��      ��                                     �                @�       $�      O       ��          O       ��      �  /     ��                                 3   ����\]      k     (�                    �        �       /     l�                                 3   ����|]  adm-create-objects  �  |�                      >      �                               �                     Carga-Temporal  ��  �          8!          ?     |!                          t!  �                      disable_UI  ��  X�                      @      �                               �   
                   enable_UI   d�  ��                      A      �                              �   	                   p-nom-aux   ̶  (�                      B      ,                              x!  	                   Texto   4�  ��              h	    , C    �	                        �	  J"                     �   �        �    ��  �             ���  �          X�  8   ����+   h�  8   ����+   x�  +  ��  8   ����*   ��  8   ����*   ��  *  ��  8   ����)   ��  8   ����)   ȸ  )  и  8   ����(   �  8   ����(   �  (  ��  8   ����'   �  8   ����'   8�  '  �  8   ����%   (�  8   ����%   @�  8   ����$   P�  8   ����$       $      8   ����#       8   ����#       p�  |�      toggleData  ,INPUT plEnabled LOGICAL    `�  ��  ��      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  ��  �  �      returnFocus ,INPUT hTarget HANDLE   ��  8�  L�      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    (�  ��  ��      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE x�  �  ��      removeAllLinks  ,   غ  �  �      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE ��  t�  ��      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    d�   �  �      hideObject  ,   �   �  ,�      exitObject  ,   �  @�  X�      editInstanceProperties  ,   0�  l�  |�      displayLinks    ,   \�  ��  ��      createControls  ,   ��  ��  ļ      changeCursor    ,INPUT pcCursor CHARACTER   ��  �  ��      applyEntry  ,INPUT pcField CHARACTER    �  (�  8�      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER �  ��  ��      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER ��  ��  ��      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE �  P�  `�      unbindServer    ,INPUT pcMode CHARACTER @�  ��  ��      startServerObject   ,   x�  ��  ��      runServerObject ,INPUT phAppService HANDLE  ��  �   �      restartServerObject ,   ܾ  �  ,�      initializeServerObject  ,   �  @�  T�      disconnectObject    ,   0�  h�  |�      destroyServerObject ,   X�  ��  ��      bindServer  ,   ��  ��  ��      processAction   ,INPUT pcAction CHARACTER   ��  �  ��      enableObject    ,   ܿ  �   �      disableObject   ,    �  4�  @�      applyLayout ,   $�  T�  `�      viewPage    ,INPUT piPageNum INTEGER    D�  ��  ��      viewObject  ,   |�  ��  ��      toolbar ,INPUT pcValue CHARACTER    ��  ��  ��      selectPage  ,INPUT piPageNum INTEGER    ��  �  ,�      removePageNTarget   ,INPUT phTarget HANDLE,INPUT piPage INTEGER �  h�  t�      passThrough ,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER  X�  ��  ��      notifyPage  ,INPUT pcProc CHARACTER ��  ��  ��      initPages   ,INPUT pcPageList CHARACTER ��  (�  D�      initializeVisualContainer   ,   �  X�  l�      initializeObject    ,   H�  ��  ��      hidePage    ,INPUT piPageNum INTEGER    p�  ��  ��      destroyObject   ,   ��  ��  ��      deletePage  ,INPUT piPageNum INTEGER    ��  �  $�      createObjects   ,   �  8�  H�      constructObject ,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE (�  ��  ��      confirmExit ,INPUT-OUTPUT plCancel LOGICAL  ��  ��  �      changePage  ,   ��  �  ,�      assignPageProperty  ,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER      � 
"     
 �%     adecomm/as-utils.w 
"   
   �    }        �
"     
   %              %                  
�    
�        �     }         �     }        �     }             �     }        �%                  �     }         �     }        �     }             �     }        �%              � 
"    
 �%              � �  �         `      $              
�    � �   �     
�             �G                      
�            � �   �
"    
 1
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �            7%               
"   
 ��           8    1� �  
 �� �   �%               o%   o           � �    �
"   
 ��           �    1� �   �� �   �%               o%   o           � �   �
"   
 ��                1� �  
 �� �   �%               o%   o           � �   �
"   
 ��           �    1� �   �� �   �%               o%   o           � �  
 �
"   
 ��               1� �   �� �   �%               o%   o           �    �
"   
 ��           |    1�    �� +   �%               o%   o           %               
"   
 ��          �    1� 3   �� C     
"   
 ��           4    1� J   �� �   �%               o%   o           � ]  e �
"   
 ��           �    1� �   �� �   �%               o%   o           � �  ? �
"   
 ��               1�    �� +   �%               o%   o           %               
"   
 ��           �    1� "   �� +   �%               o%   o           %               
"   
 ��               1� 4   �� +   �%               o%   o           %              
"   
 ��          �    1� A   �� +     
"   
 ��           �    1� P  
 �� +   �%               o%   o           %               
"   
 ��           H	    1� [   �� �   �%               o%   o           � �    �
"   
 ��          �	    1� c   �� C     
"   
 ��           �	    1� s   �� �   �%               o%   o           � �  t �
"   
 ��          l
    1� �  
 �� C     
"   
 ��           �
    1� 	   �� �   �%               o%   o           �   � �
"   
 ��               1� �   �� �   �%               o%   o           � �    �
"   
 ��           �    1� �  
 �� �   �%               o%   o           %               
"   
 1�               1� �   1� +   �%               o%   o           %               
"   
 1�           �    1� �   1� �   �%               o%   o           � �    1
"   
 1�           �    1� �   1� �   �%               o%   o           o%   o           
"   
 2�           x    1� �  
 2� �   �%               o%   o           � �    0
"   
 1�           �    1�    1�   	 �%               o%   o           �   / 2
"   
 ��          `    1� L   ��   	   
"   
 0�           �    1� ^   0�   	 �o%   o           o%   o           � �    0
"   
 ��              1� q   ��   	   
"   
 1�           L    1� �   1�   	 �o%   o           o%   o           � �    1
"   
 ��          �    1� �   �� +     
"   
 ��          �    1� �   ��   	   
"   
 ��          8    1� �   ��   	   
"   
 ��          t    1� �   ��   	   
"   
 ��           �    1� �   �� +   �o%   o           o%   o           %              
"   
 ��          ,    1� �   ��   	   
"   
 ��          h    1� �  
 �� �     
"   
 ��          �    1� �   ��   	   
"   
 ��          �    1�    ��   	   
"   
 ��              1�    ��   	   
"   
 ��          X    1� /   ��   	   
"   
 ��          �    1� >  	 ��   	   
"   
 ��          �    1� H   ��   	   
"   
 ��              1� [   ��   	   
"   
 1�           H    1� r   1� �   �%               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 2
"   
   
"   
 �(�  L ( l       �            �� ~   � P   �            �@    
� @  , 
�       (    �� �     p�               �L
�    %              � 8      4    � $         � �          
�    � �     
"   
 �� @  , 
�       D    �� �  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 0�           �    1� �  
 0� �   �%               o%   o           � �    0
"   
 0�           d    1� �  
 0� �   �%               o%   o           o%   o           
"   
 1�           �    1� �   1� C   �%               o%   o           o%   o           
"   
 1�           \    1� �   1� +   �%               o%   o           %               
"   
 1�           �    1� �   1� +   �%               o%   o           %               
"   
 ��           T    1� �   �� �   �%               o%   o           � �    1
"   
 ��           �    1� �   �� +   �%               o%   o           %              
"   
 ��           D    1� �   �� +   �%               o%   o           o%   o           
"   
 2�           �    1�    2� �   �%               o%   o           o%   o           
"   
 1�           <    1�   	 1� �   �%               o%   o           � �    0
"   
 1�           �    1� #   1� �   �%               o%   o           o%   o           
"   
 2�           ,    1� 7   2� �   �%               o%   o           o%   o           
"   
 1�           �    1� F   1� +   �%               o%   o           %               
"   
 1�           $    1� V   1� +   �%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 2�           �    1� b   2�   	 �%               o%   o           � �    2
"   
 2�           h    1� o   2�   	 �%               o%   o           � �    2
"   
 0�           �    1� }   0� +   �%               o%   o           %               
"   
 ��           X    1� �   ��   	 �%               o%   o           � �    0
"   
 2�           �    1� �   2�   	 �%               o%   o           � �    �
"   
 ��           @    1� �   �� +   �%               o%   o           %               
"   
 1�           �    1� �   1�   	 �%               o%   o           � �    �
"   
 1�           0     1� �   1�   	 �%               o%   o           � �    1
"   
 2�           �     1� �   2�   	 �%               o%   o           � �    1
"   
 2�           !    1� �   2�   	 �%               o%   o           o%   o           
"   
 0�           �!    1� �   0�   	 �%               o%   o           � �    2
"   
 ��           "    1�     ��   	 �%               o%   o           � �    0
"   
 2�           |"    1�   	 2� �   �%               o%   o           %               
"   
 ��           �"    1�    �� �   �%               o%   o           %               
"   
 ��           t#    1� !   �� +   �%               o%   o           o%   o           
"   
 1�           �#    1� 2   1� +   �%               o%   o           o%   o           
"   
 2�           l$    1� A   2� +   �%               o%   o           %               
"   
 2�           �$    1� O   2� +   �%               o%   o           %               
"   
 0�           d%    1� `   0� +   �%               o%   o           %               
"   
 ��           �%    1� u   �� �   �%               o%   o           %       
       
"   
 ��           \&    1� �   �� �   �%               o%   o           o%   o           
"   
 1�           �&    1� �   1� �   �%               o%   o           %              
"   
 1�           T'    1� �   1� �   �%               o%   o           o%   o           
"   
 0�           �'    1� �   0� �   �%               o%   o           %              
"   
 0�           L(    1� �   0� �   �%               o%   o           o%   o           
"   
 2�           �(    1� �   2� �   �%               o%   o           %              
"   
 2�           D)    1� �   2� �   �%               o%   o           o%   o           
"   
 ��           �)    1� �   ��   	 �%               o%   o           � �    1P �L 
�H T   %              �     }        �GG %              
"   
 2�           �*    1� �   2� �   �%               o%   o           %               
"   
 2�           +    1� �   2� �   �%               o%   o           o%   o           
"   
 ��           �+    1�    �� �   �%               o%   o           � �    1
"   
 0�           �+    1�    0� �   �%               o%   o           � '  - �
"   
 0�           h,    1� U   0� �   �%               o%   o           � �    0
"   
 2�           �,    1� l   2� �   �%               o%   o           � �   0
"   
 ��          P-    1� �   �� C     
"   
 1�           �-    1� �   1� �   �%               o%   o           � �    2
"   
 ��           .    1� �  
 �� C     
"   
 ��          <.    1� �   �� C     
"   
 ��           x.    1� �   ��   	 �%               o%   o           � �    1
"   
 0�           �.    1� �   0� �   �%               o%   o           � �    �
"   
 0�           `/    1� �   0� C   �%               o%   o           o%   o           
"   
 2�           �/    1�    2� �   �%               o%   o           �   ! 1
"   
 1�           P0    1� 8   1� �   �%               o%   o           � �    2
"   
 ��           �0    1� E   �� �   �%               o%   o           � X   1
"   
 ��           81    1� g  	 �� �   �%               o%   o           o%   o           
"   
 1�           �1    1� q   1� +   �%               o%   o           %               
"   
 ��          02    1� }   �� C     
"   
 0�           l2    1� �   0� �   �%               o%   o           � �   0
"   
 1�           �2    1� �   1�   	 �%               o%   o           � �    0
"   
 2�           T3    1� �   2�   	 �%               o%   o           � �    1
"   
 ��          �3    1� �   �� C     
"   
 ��          4    1� �   ��   	   
"   
 ��           @4    1� �   �� +   �o%   o           o%   o           %               
"   
 ��          �4    1�    �� +     
"   
 ��          �4    1�    ��   	   
"   
 ��          45    1� ,   ��   	   
"   
 ��          p5    1� ?   ��   	   
"   
 ��          �5    1� P   ��   	   
"   
 ��          �5    1� a   ��   	   
"   
 ��          $6    1� r   �� C     
"   
 2�           `6    1� �   2� �   �%               o%   o           � �  4 2
"   
 ��          �6    1� �   �� C     
"   
 ��          7    1� �   �� C     
"   
 ��          L7    1� �   �� C     
"   
 ��          �7    1� �   ��   	   
"   
 ��          �7    1�    ��   	   
"   
 ��           8    1�    ��   	   
"   
 ��          <8    1� 1   �� +     
"   
 ��           x8    1� >   ��   	 �%               o%   o           � �    0
"   
 1�           �8    1� L   1�   	 �%               o%   o           � �    �
"   
 2�           `9    1� X   2�   	 �%               o%   o           � �    1
"   
 2�           �9    1� m   2�   	 �%               o%   o           � �    2
"   
 2�           H:    1� �   2� +   �%               o%   o           %               
"   
 2�           �:    1� �   2� +   �%               o%   o           o%   o           
"   
 1�           @;    1� �   1� +   �%               o%   o           %               
"   
 0�           �;    1� �   0� +   �%               o%   o           %               
"   
 0�           8<    1� �   0� +   �%               o%   o           o%   o           
"   
 1�           �<    1� �   1� +   �%               o%   o           %               
"   
 ��          0=    1� �   ��   	   
"   
 1�           l=    1� �   1� +   �%               o%   o           %              
"   
 ��          �=    1�    ��   	   
"   
 ��          $>    1�    ��   	   
"   
 ��          `>    1� !  
 ��   	   
"   
 0�           �>    1� ,   0�   	 �%               o%   o           � �   1
"   
 0�           ?    1� >   0�   	 �%               o%   o           � �    0
�             �G "    �%     start-super-proc ��%     adm2/smart.p J�P �L 
�H T   %              �     }        �GG %              
"   
   �       8@    6� ~     
"   
   
�        d@    8
"   
   �        �@    ��     }        �G 4              
"   
 ߱G %              G %              %p e `   LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout �
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �A    �� ~   � P   �        �A    �@    
� @  , 
�       �A    �� �   �p�               �L
�    %              � 8      �A    � $         � �          
�    � �   �
"   
 �p� @  , 
�        C    �� J   �p�               �L"    , �   � {   1� }   ��     }        �A      |    "      � {   0%              (<   \ (    |    �     }        �A�    �A"    1    "    �"    1  < "    �"    1(    |    �     }        �A�    �A"    1
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �D    �� ~   � P   �        �D    �@    
� @  , 
�       �D    �� �   �p�               �L
�    %              � 8      �D    � $         � �          
�    � �   �
"   
 �p� @  , 
�       F    �� �  
 �p�               �L"    , 
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 �(�  L ( l       �        �F    �� ~   � P   �        �F    �@    
� @  , 
�       �F    �� �   �p�               �L
�    %              � 8      �F    � $         � �   �     
�    � �   �
"   
 �p� @  , 
�       �G    �� 3   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
   
"   
 2
"   
   
"   
   (�  L ( l       �        �H    �� ~   � P   �        �H    �@    
� @  , 
�       �H    �� �     p�               �L
�    %              � 8      �H    � $         � �          
�    � �     
"   
 �p� @  , 
�       �I    �� �  
 �p�               �L%     SmartDialog 
"   
   p� @  , 
�       $J    �� �     p�               �L% 
    DIALOG-BOX  
"   
  p� @  , 
�       �J    �� �    p�               �L%               
"   
  p� @  , 
�       �J    �� ^    p�               �L(        � �      � �      � �      �     }        �A
�H T   %              �     }        �GG %              
"   
 0 (   � 
"   
 �    �        �K    �� ~   �
"   
   � 8      L    � $         � �          
�    � �   �
"   
   �        lL    �
"   
   �       �L    /
"   
   
"   
   �       �L    6� ~     
"   
   
�        �L    8
"   
   �        M    �
"   
   �       $M    �
"   
   p�    � �   0
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 �    �        �M    �A"    �A
"   
   
�        4N    �@ � 
"   
 0"      �       }        �
"   
 �%              %                "    �%     start-super-proc ��%     adm2/appserver.p �0�    � )     
�    �     }        �%               %      Server  - �     }        �    "    2� �    �%                   "    2� �    �%      NONE    p�,  8         $     "    �        � C   �
�    
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        tP    �� ~   � P   �        �P    �@    
� @  , 
�       �P    �� �   �p�               �L
�    %              � 8      �P    � $         � �          
�    � �   �
"   
 �p� @  , 
�       �Q    �� #   �p�               �L"    , p�,  8         $     "    �        � Q   �
�     "    �%     start-super-proc ��%     adm2/visual.p ��   � �     � u     � w  <   
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        S    �� ~   � P   �        S    �@    
� @  , 
�       S    �� �   �p�               �L
�    %              � 8      (S    � $         � �          
�    � �   �
"   
 �p� @  , 
�       8T    �� �   �p�               �L"    , � 
"    
 �%     contextHelp 
"    
   
�    
�    %     processAction   
�    %     CTRL-PAGE-UP I�%     processAction   
�    %     CTRL-PAGE-DOWN  "    �%     start-super-proc �%     adm2/containr.p %     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents 1%      initializeDataObjects 10 0   A    �    � �   1
�    �    �A    �    � �     
�    �    �%     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents 2%     buildDataRequest ent0 A    �    � �   �
�    � 5   2%     modifyListProperty 
�    
�    %      Add     %     SupportedLinks %      ContainerToolbar-Target %               
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 1(�  L ( l       �        4X    �� ~   � P   �        @X    �@    
� @  , 
�       LX    �� �   �p�               �L
�    %              � 8      XX    � $         � �   �     
�    � �   �
"   
 �p� @  , 
�       hY    �� �   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 �(�  L ( l       �        Z    �� ~   � P   �         Z    �@    
� @  , 
�       ,Z    �� �   �p�               �L
�    %              � 8      8Z    � $         � �   �     
�    � �   �
"   
 �p� @  , 
�       H[    �� �   �p�               �L%              �             I%               �             �%              % 	    END-ERROR 1%      Texto   �     }        � `     @     ,         � z  (   G %       
       � �  &   G %       
       � �  & �% 
    disable_UI 
�    %                0   � 
�        
�             � 
%   
           
�             � 
�    %     createObjects   %     initializeObject �%     destroyObject   %               %               %               %               ,    %              %       ?B     "    �"     �&    &    &    &        %              %               V �  "     �"     �"  
  �"    �&    &    &    &    &    &    L    0        %              %              %              %              �            B p     \     H     4          � J          " #     � P      � S      " #     � \   
   " #     " 	    �" #   �&    &    &    &        %              %              " $     " $     " #   �"     �"     �" #   �" #   �&    &    &    &    &    &    &    &    &    &    h    L    0        %              %              %              %              %              (        " %     " &     %               %              " %         "    1� �     �% 	    p-nom-aux �    "    1� �     �" #     �   � �      G %              "    ��   � �      G %       
       "    ��   � �      G %              "    � " %     "      %              " %     %               %              " %     %               %              " %     %               "      %              %               " %     %              %               " %     %              %               " %     � (   \ (   X ( (       "    �%                   "    �%                   "    �%                  "    �%                   "      "           "      "           "      "           "      "          "       %                  "       "      %               %                   "       "      %               %               X (   ( (       "      %                  " %     %                  " %     %                  "      %              %                   "    �%                @   "      U ,   � �          %               "       H                "    ߱� �          " %     � �      � �      "      "       "       " #     %              " #     %              " #     %              " %     %              " %     %              " %     %              " %   ��%              " %     %              " %     %              " %   ? %              " %     %              " %     %       	       "      %       
       "      %              "      %              "  	    %              "       %              " #     %                   "      %                   "       %                  " #     � �      "      "       " #     %              " #     %              " #     %              � �      %       
       "  	    %              "       %              " #   ��%                   "       %              (         "      %                   " #     � �      "      "       " #     %              " #     %              " #     %              " #     %       
       "  	    %              "       %              " #   ��%                   "       %              %               �            B� �       "    2"  
  �"    �"    �" %   �� �      " %   �" 
    �&    &    &    &        %              %              * (   " (     � !     " %   �"     �&    &    &    &        %              %              * )   " )     � @!     " %   �" 	    �&    &    &    &        %              %              * *   " *     " %   �" %   �" 	    �&    &    &    &    &    &    0        %              %              %              * +   " +     � �!  
   " ,     � �!     � �!     � �!         " ,   1%               %               %     Carga-Temporal  "    �"     �&    &    &    &        %              %               * '   � �!     %               " ,     � �!  F   "    �"     �&    &    &    &        %              %              4  '     %              4  '     %              4  '     %              4  '     %              � P     � "    %      x(1)    4  '   1)%              � "    � "    %      x(1)    4  '   1)%       	       � "    � "    %      x(1)    4  '   1)%              � "  
  � "    %      x(1)    4  '   1)%       
       � "    � "    %      x(1)    4  '   1)%              � ""    � "    %      x(1)    4  '   1)%              � '"    � "    %      x(1)    4  '   1)%              � '"    � "    %      x(1)    � "     %      x(1)    � 8"                     �           �   l       ��                 V  z  �               \�1                    O   ����    e�          O   ����    R�          O   ����    ��        $  e  �   ���                       |K     
                    � ߱              f  (  �      �K      4   �����K                �                      ��                  g  y                  1                       g  8  �  �  h   L            j  �  `      xL      4   ����xL                p                      ��                  k  x                  ��1                       k  �  �  o   l      ,                                 �  �   m  �L      �  �   n  �L      $  $  o  �  ���                       �L     
                    � ߱        8  �   p  M      L  �   q  0M      `  �   t  PM          $   w  �  ���                       �M  @         lM              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �  �  �               ��1                    O   ����    e�          O   ����    R�          O   ����    ��      �                      �          �  $  �    ���                       �M     
                    � ߱                  �  �                      ��                   �  �                  �1                     �  4      4   �����M      $  �  �  ���                       @N     
                    � ߱        �    �  4  D      TN      4   ����TN      /  �  p                               3   ����hN  �  �   �  tN          O   �  ��  ��  �N                               , �                          
                               �      ��                            ����                                                        �   l       ��                  (  /  �               (2                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                                            4          �   l       ��\"               5  �  �               p*2                    O   ����    e�          O   ����    R�          O   ����    ��      �]                     �]                     �]                     �]                         � ߱        �  $  >  �   ���                                     $          �  �      ��                  D  H                �*2                �     D  `      O   ����  e�          O   ����  R�          O   ����  ��      |  $  E  P  ���                       �]                         � ߱              F  `  p      �^      A   F      ! �   ��         �  P^                                        ^   $^                   L  @           0^  @^           8^  H^         �               ,        4   �����^      O   G  �� ��                  �  <"      �  �      ��                 J  �  �              �0                �      J  �      0  �       ��                            7   ����   #      ��               �^    �            �                  6   J       #    ��         �  �^    �            �                                                        �^   �^   �^   �^                   l  `           �^  �^  �^           �^  �^  �^                      0   H        O   ����  e�          O   ����  R�          O   ����  ��         $   N  �  ���                       h_  @         T_              � ߱        0  A  Q       $ �   ��         p  $`                                         �_   �_                   �  �           `  `           `  `         �            �   �    T`                     ``       	       	           � ߱        �  $  S  �  ���                         4  �      �	  L"      �	  �	  p	  ��           |a  V  �  �	              ��2                �     V  \        T       ��    h	                      7   ����   %      &                �`    �            �                  6   V       % �   &          �  �`    �            �                                                        l`   x`   �`   �`   �`                 T	  H	           �`  �`  �`  �`  �`           �`  �`  �`  �`  �`         �            	   (	        % &     8   V  &       O   ����  e�          O   ����  R�          O   ����  ��       
  $  \  �	  ���                       �a                         � ߱        �
    ]  <
  L
      �a      4   �����a      /   ]  x
                                 3   �����a      ^  �
  �
      b      4   ����b      $  ^  �
  ���                       0b                         � ߱        d  $  `  8  ���                       <b                         � ߱        �  $  a  �  ���                       tb                         � ߱          $  b  �  ���                       �b                         � ߱        �    d  0  @    �b      4   �����b      p   e  �b  \      r  �  �      c                �                      ��                  f  i                  �2                       f  l  @  $  g    ���                       c                         � ߱            $  h  l  ���                        c                         � ߱        �       4c                $                      ��                  j  m                  d�2                       j  �  |  $  k  P  ���                       Hc                         � ߱            $  l  �  ���                       Tc                         � ߱            P     hc                `                      ��                  n  q                   1                       n  �  �  $  o  �  ���                       |c                         � ߱            $  p  �  ���                       �c                         � ߱            p   t  �c  ,      �  h  �     �c                �                      ��                  u  x                  �1                       u  <    $  v  �  ���                       �c                         � ߱            $  w  <  ���                       �c                         � ߱        �  �     �c                �                      ��                  y  |                  $1                       y  x  L  $  z     ���                       �c                         � ߱            $  {  x  ���                       d                         � ߱                  d  	              0                      ��             	     }  �                  �1                       }  �  �  $  ~  \  ���                       $d                         � ߱            $    �  ���                       8d                         � ߱              �  �  x      Dd      4   ����Dd  
              �                      ��                  �  �                  �1                       �    �  $  �  �  ���                        e                         � ߱        8  $  �    ���                        e                         � ߱        �  $  �  d  ���                       @e                         � ߱          $  �  �  ���                       `e                         � ߱               �    T              4   �  �e  $  3   �  �e  t  2   �     D  3   �  �e  �  2   �     d  3   �  �e  �  2   �     �  3   �  �e  �  2   �     �  3   �  f      2   �     �  3   �  $f      2   �     �    �  �  l      8f      4   ����8f                |                      ��                  �  �                  �s1                       �     �  $  �  �  ���                       �f                         � ߱        X    �  �         �f      4   �����f      $  �  ,  ���                       (g                         � ߱            $  �  �  ���                       |g                         � ߱          9   �  '   �g      '               �g      '               �g      '               �g      '    h         h      '    (h         <h      '    Hh         \h      '    hh         |h      '    �h         �h      '    �h         �h      '    �h         �h      '    �h         �h      '    i         i      '    (i         <i      '    Hi         \i      '    hi         |i      '    �i         �i     '    �i         �i     '    �i         �i      '    �i         �i      '    j         j      '    (j             � ߱        H  V   �  �  ���                        �  $  �  t  ���                       <j                         � ߱            $  �  �  ���                       dj                          � ߱        @    �    �      �j      4   �����j                �                      ��                  �  �                  ��1                       �  $  �  9   �  '   �j      '               �j      '               �j      '    �j         �j      '    �j         k      '    k         $k      '    0k         Dk      '    Pk         dk      '    pk         �k      '    �k             � ߱        �  V   �  �  ���                            $  �    ���                       �k                          � ߱        �     �  \  �      �k      4   �����k                �                      ��                  �  �                  `�1                       �  l     9   �  '   l      '               (l      '               4l      '    @l         Tl      '    `l         tl      '    �l         �l      '    �l         �l      '    �l         �l      '    �l         �l      '     m             � ߱        0   V   �  �  ���                            $  �  \   ���                       m                          � ߱            $  �  �   ���                       <m                         � ߱            $   �  !  ���                       dm  @         Pm              � ߱                       l!                                   ���    ! "     ��                             ��                             ��                              ��        *                   ��                            ����                                =   �  '   �  =   �  '       $                  �           �   l       ��                  �  �  �               H�2                    O   ����    e�          O   ����    R�          O   ����    ��             �  �� �                   ��                              ��        *                  ����                                                      �   l       ��                  �  �  �               ��1                    O   ����    e�          O   ����    R�          O   ����    ��      pm  �           |m  �          �m  �          �m  �              � ߱        �  Z   �  �    �                            �               �              �              �              �              � ߱        �  h   �  0   �                            
   �  �� �                  ��                              ��        *                  ����                                            �           �   l       ��l               �    �               |�1                    O   ����    e�          O   ����    R�          O   ����    ��          p   �  �m  �   �    �  L     �m                \                      ��                  �                    _2                       �  �   $  A  �       ( �   ��         �  �m                                        �m   �m                                �m  �m           �m  �m         �            �   �            @  P       n      4   ���� n      $    |  ���                       (n                         � ߱        �  $     4n                4                      ��                    	                  ��1                         �  �  A         ) �   ��         �  xn                                        @n   Ln                   �  �           Xn  hn           `n  pn         �            �   �              (      �n      4   �����n      $    T  ���                       �n                         � ߱            �     �n                                      ��                  
                    ��2                       
  �  �  A         * p   ��         \   o                                        �n   �n                   �  �           �n  �n           �n  �n         �            �   �            �         0o      4   ����0o      $    ,  ���                       8o                         � ߱                      �                      ��                                      L�1                         X  �  A         + <   ��         $  �o                                        Do   Po   \o                 �  �           ho  xo  �o           po  �o  �o         �            X   p            �  �      �o      4   �����o      $       ���                       �o                         � ߱          ��                            ����                            t  +  |  *  �  )      (                  �           �   l       ��<
               #  Y  �               Ԑ2                    O   ����    e�          O   ����    R�          O   ����    ��        $  -  �   ���                       �o      ,                   � ߱        L  r   /          ,  ,     (p          D  p  =  p  p  �    8  h  x      4p      4   ����4p      O   8  ��  ��  \p  �  /   ;  �                                 3   ����pp  �  A  <       ' 0   ��        	   �p                                         �p   �p                   �  t           �p  �p           �p  �p         �            L   `    �    >  �  ,      �p      4   �����p                <                      ��                  >  A                  �>2                       >  �  �  	  ?  p                                        3   ���� q      O   @  ��  ��  q  �     C  �  �                                                    3   ���� q  �  Q   D           ,q                                               �      �          �  �      ��                  G  U  �              <?2                	     G  0      �  (  �                                7   ����    '      ��          
     pq    �x          x                  6   G       ' �   ��        
 �  pq    �x          x                                                        8q   Dq                      �           Pq  `q           Xq  hq         �            �   �          <      �          0          '                                                                                                                                                                                           J   G          \    ��                                                           �q  �q  �q                      D           �  �        O   ����  e�          O   ����  R�          O   ����  ��          Q   J  �         r  0r         <r  Hr         \r  �r         �r  �r         �r  �r         �r  �r         �r   s         ,s  8s         Ls  ps         |s  �s         �s  �s         �s  �s         �s  t         t  (t         <t  `t         lt  xt         �t  �t                                     $	  P   V             	  W  X	                                        3   �����t             ,  �	          �	  �	   , �	                                                            ,     ��                             ��                            ����                                '      �    d d     �   ��  �  � �       2  �                                  *   	                                                           
   d     D                                                                 t  L� ��                                              "           !     U  
              G  d   M  x    P   �� �Q                                                           P"  G     �  �� �X                                             0           �     `                      �  �  �  �  �  �  �  �  �  �           P   �� �Q                                                           ]"  G     �  �� �X                                             2                `                      �  �  �  �  �  �  �  �  �  �           `  h�                                                           �        $                  \  �q��                                 �          .       j"      <        @      `  @q                                                          �        $                  \  @q �                                 �                  s"      �        B     
 X  (
�Q                                             4                c      P ��� �A                                                        j        D                                                                                            TXS appSrvUtils ASSIGNFOCUSEDWIDGET ASSIGNWIDGETVALUE ASSIGNWIDGETVALUELIST BLANKWIDGET CLEARWIDGET DISABLERADIOBUTTON DISABLEWIDGET ENABLERADIOBUTTON ENABLEWIDGET FORMATTEDWIDGETVALUE FORMATTEDWIDGETVALUELIST HIDEWIDGET HIGHLIGHTWIDGET RESETWIDGETVALUE TOGGLEWIDGET VIEWWIDGET WIDGETHANDLE WIDGETLONGCHARVALUE WIDGETISBLANK WIDGETISFOCUSED WIDGETISMODIFIED WIDGETISTRUE WIDGETVALUE WIDGETVALUELIST report B-Cuentas cb-ctas l-immediate-display x-Raya s-user-id x-Detalle i OKpressed x-con-reg x-smon PTO pinta-mes x-debe x-haber x-totdebe x-tothabe x-codope x-nom-ope x-nroast x-fecha x-glodoc s-NroMes s-periodo s-codcia s-nomcia cb-codcia cl-codcia pv-codcia t-d t-h s-task-no Btn_Cancel img/exit.ico BUTTON-1 img\tbldat COMBO-BOX-Mes-1 01 02 03 04 05 06 07 08 09 10 11 12 COMBO-BOX-Mes-2 f-Mensaje x-codmon gDialog Libro Diario General Soles D�lares ->,>>>,>>9 99 X(256) Moneda: DISABLEPAGESINFOLDER ENABLEPAGESINFOLDER GETCALLERPROCEDURE GETCALLERWINDOW GETCONTAINERMODE GETCONTAINERTARGET GETCONTAINERTARGETEVENTS GETCURRENTPAGE GETDISABLEDADDMODETABS GETDYNAMICSDOPROCEDURE GETFILTERSOURCE GETMULTIINSTANCEACTIVATED GETMULTIINSTANCESUPPORTED GETNAVIGATIONSOURCE GETNAVIGATIONSOURCEEVENTS GETNAVIGATIONTARGET GETOUTMESSAGETARGET GETPAGENTARGET GETPAGESOURCE GETPRIMARYSDOTARGET GETREENABLEDATALINKS GETRUNDOOPTIONS GETRUNMULTIPLE GETSAVEDCONTAINERMODE GETSDOFOREIGNFIELDS GETTOPONLY GETUPDATESOURCE GETUPDATETARGET GETWAITFOROBJECT GETWINDOWTITLEVIEWER GETSTATUSAREA PAGENTARGETS SETCALLEROBJECT SETCALLERPROCEDURE SETCALLERWINDOW SETCONTAINERMODE SETCONTAINERTARGET SETCURRENTPAGE SETDISABLEDADDMODETABS SETDYNAMICSDOPROCEDURE SETFILTERSOURCE SETINMESSAGETARGET SETMULTIINSTANCEACTIVATED SETMULTIINSTANCESUPPORTED SETNAVIGATIONSOURCE SETNAVIGATIONSOURCEEVENTS SETNAVIGATIONTARGET SETOUTMESSAGETARGET SETPAGENTARGET SETPAGESOURCE SETPRIMARYSDOTARGET SETREENABLEDATALINKS SETROUTERTARGET SETRUNDOOPTIONS SETRUNMULTIPLE SETSAVEDCONTAINERMODE SETSDOFOREIGNFIELDS SETTOPONLY SETUPDATESOURCE SETUPDATETARGET SETWAITFOROBJECT SETWINDOWTITLEVIEWER GETOBJECTTYPE SETSTATUSAREA GETALLFIELDHANDLES GETALLFIELDNAMES GETCOL GETDEFAULTLAYOUT GETDISABLEONINIT GETENABLEDOBJFLDS GETENABLEDOBJHDLS GETHEIGHT GETHIDEONINIT GETLAYOUTOPTIONS GETLAYOUTVARIABLE GETOBJECTENABLED GETOBJECTLAYOUT GETROW GETWIDTH GETRESIZEHORIZONTAL GETRESIZEVERTICAL SETALLFIELDHANDLES SETALLFIELDNAMES SETDEFAULTLAYOUT SETDISABLEONINIT SETHIDEONINIT SETLAYOUTOPTIONS SETOBJECTLAYOUT SETRESIZEHORIZONTAL SETRESIZEVERTICAL GETOBJECTTRANSLATED GETOBJECTSECURED CREATEUIEVENTS GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE , prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDialog ContainerType DIALOG-BOX PropertyDialog adm2/support/visuald.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout SupportedLinks Data-Target,Data-Source,Page-Target,Update-Source,Update-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext ObjectLayout LayoutOptions ObjectEnabled LayoutVariable DefaultLayout DisableOnInit EnabledObjFlds EnabledObjHdls FieldSecurity SecuredTokens AllFieldHandles AllFieldNames MinHeight MinWidth ResizeHorizontal ResizeVertical ObjectSecured ObjectTranslated PopupButtonsInFields ColorInfoBG INTEGER ColorInfoFG ColorWarnBG ColorWarnFG ColorErrorBG ColorErrorFG BGColor FGColor FieldPopupMapping CurrentPage PendingPage ContainerTarget ContainerTargetEvents exitObject,okObject,cancelObject,updateActive ContainerToolbarSource ContainerToolbarSourceEvents toolbar,okObject,cancelObject OutMessageTarget PageNTarget PageSource FilterSource UpdateSource UpdateTarget CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState StartPage RunMultiple WaitForObject DynamicSDOProcedure adm2/dyndata.w RunDOOptions InitialPageList WindowFrameHandle Page0LayoutManager MultiInstanceSupported MultiInstanceActivated ContainerMode SavedContainerMode SdoForeignFields NavigationSource NavigationTarget PrimarySdoTarget NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter CallerWindow CallerProcedure CallerObject DisabledAddModeTabs ReEnableDataLinks WindowTitleViewer UpdateActive InstanceNames ClientNames ContainedDataObjects ContainedAppServices DataContainer HasDbAwareObjects HasDynamicProxy HideOnClose HideChildContainersOnClose HasObjectMenu RequiredPages RemoveMenuOnHide ProcessList PageLayoutInfo PageTokens DataContainerName WidgetIDFileName ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService cFields adm2/visual.p   x-codmon COMBO-BOX-Mes-1 COMBO-BOX-Mes-2 BUTTON-1 Btn_Cancel CTRL-PAGE-UP CTRL-PAGE-DOWN adm2/containr.p Add initializeDataObjects getSupportedLinks data-target data-source buildDataRequest containertoolbar-target ContainerToolbar-Target END-ERROR iStartPage A SmartDialog is not intended to be run  Persistent or to be placed in another  SmartObject at AppBuilder design time. ADM-CREATE-OBJECTS x-Orden w-report Tabla de Reportes cb-cmov Cabecera de Mov. Contables Mes:  99  Libro:   Asiento:  cb-oper Operaciones Contables cb-dmov Detalle Mov.Contable     ($ ZZ,ZZZ,ZZ9.99 ) A ******* A N U L A D O ******* CARGA-TEMPORAL DISABLE_UI ENABLE_UI @CL gn-clie Maestro de Clientes @PV gn-prov Maestro de Proveedores @CT Plan de Cuentas Contable cb-auxi Codigos Auxiliares P-NOM-AUX x-Archivo x-Rpta Diario.txt Texto *.txt .txt Fin de archivo DMES|DNUMASIOPE|DNUMCTACON|DFECOPE|DGLOSA|DCENCOS|DDEBE|DHABER|DINTREG | X(6) X(8) 99/99/9999 x(50) X(2) ->>>>>>>>>>>9.99 Proceso Terminado TEXTO Desde el mes Hasta el mes Button 1 Cancel REPO01 CMOV01 oper01 DMOV00 idx01 idx00 idx02 �   #      �)      % �8   ��      0         pcProp      ��      P         pcProp      ��      p         plCancel    �   ��      �         pcProcName  �   ��      �        
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
 hTarget X  ��      L        pcMessage       ��      p        pcMessage       ��      �        plEnabled             �     cType       �     6   �          �                  getObjectType   Z	  r	  t	  ,          
   hReposBuffer    L        @  
   hPropTable  h        `  
   hBuffer           |  
   hTable  �  �     7             �                  adm-clone-props e  f  g  h  j  k  l  m  n  o  p  q  t  w  x  y  z              
   hProc             <        pcProcName  �  �  	   8     $      x                  start-super-proc    �  �  �  �  �  �  �  �  �  H  �     9                                   �  �  	     :                                   �  �  �  L	     ;                                   �  �  	  �	     <                                   �  �  T	  �	     =                                   �  �  �  �	  
     >               �	                  adm-create-objects  /             $
     x-Orden      "    <
  w-report    �	  �
  N   ?   
      ,
  x
                  Carga-Temporal  >  D  E  F  G  H  J  N  Q  S  V  \  ]  ^  `  a  b  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  t  u  v  w  x  y  z  {  |  }  ~    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  H
  �     @               �                  disable_UI  �  �  �  @     A               4                  enable_UI   �  �  �  �    �     B               �                  p-nom-aux   �  �  �            	  
                           ,      �     x-Archivo       ,           x-Rpta  P  T     C   �          L                  Texto   -  /  8  ;  <  >  ?  @  A  C  D  G  J  U  V  W  Y    �  �      �                            �          �  
   appSrvUtils        �     l-immediate-display ,       $     x-Raya  L        @     s-user-id   l       `     x-Detalle   �       �     i   �       �     OKpressed   �       �     x-con-reg   �       �     x-smon  �       �     PTO     	        pinta-mes   4       ,     x-debe  P       H     x-haber p       d     x-totdebe   �       �     x-tothabe   �       �     x-codope    �    	   �     x-nom-ope   �    
   �     x-nroast                x-fecha ,             x-glodoc    L        @     s-NroMes    l        `     s-periodo   �        �     s-codcia    �        �     s-nomcia    �  	 	     �     cb-codcia   �  
 
     �     cl-codcia                 pv-codcia   $             t-d <       8     t-h \       P     s-task-no   �    
   p     COMBO-BOX-Mes-1 �       �     COMBO-BOX-Mes-2 �       �     f-Mensaje   �       �     x-codmon            �  
   gshAstraAppserver   4           
   gshSessionManager   X        H  
   gshRIManager    �        l  
   gshSecurityManager  �        �  
   gshProfileManager   �        �  
   gshRepositoryManager             �  
   gshTranslationManager   $          
   gshWebManager   H        8     gscSessionId    l        \     gsdSessionObj   �        �  
   gshFinManager   �        �  
   gshGenManager   �        �  
   gshAgnManager   �        �     gsdTempUniqueID              gsdUserObj  D        0     gsdRenderTypeObj    l        X     gsdSessionScopeObj  �       �  
   ghProp  �       �  
   ghADMProps  �       �  
   ghADMPropsBuf   �       �     glADMLoadFromRepos              glADMOk 0       $  
   ghContainer P       D     cObjectName l       d     iStart  �       �     cAppService �       �     cASDivision �       �     cServerOperatingMode    �       �     cFields               iStartPage  0     C  $  B-Cuentas   H   #    @  cb-cmov `   $    X  cb-oper x   %    p  cb-dmov �  ! '   �  w-report    �  " (    �  gn-clie �  # )    �  gn-prov �  $ *    �  cb-ctas     % +    �  cb-auxi          9   �  �  �  �  �  �  �  (  )  *  +  B  N  O  P  R  T  U  V  Z  [  ^  _  `  a  c  e  g  i  j  k  n  p  q  s  t  u  v  w  }    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  
  &
  '
  )
  *
  +
  ,
  -
  .
  /
  0
  1
  2
  3
  4
  5
  6
  7
  8
  9
  :
  ;
  <
  =
  >
  ?
  @
  A
  B
  C
  D
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
         s    �  �  �  �  �  �  �  �  �  �  �    +  G  I  ^  �  �  �    )  *  +  .  /  0  7  8  U  i  �      #  -  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  e  �  �  �  �      	  
                          �� $ C:\Progress\OpenEdge\src\adm2\dialogmn.i �  f!  C:\Progress\OpenEdge\src\adm2\containr.i   � # %C:\Progress\OpenEdge\src\adm2\custom\containrcustom.i    L  ��  C:\Progress\OpenEdge\src\adm2\visual.i   �  # " %C:\Progress\OpenEdge\src\adm2\custom\visualcustom.i  �  �<  C:\Progress\OpenEdge\src\adm2\appserver.i      �� ! %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   <  I�  C:\Progress\OpenEdge\src\adm2\smart.i    �  Ds   C:\Progress\OpenEdge\gui\fn  �  tw  %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   �  Q.  C:\Progress\OpenEdge\gui\set   ��  C:\Progress\OpenEdge\src\adm2\cntnprop.i D  ��  %C:\Progress\OpenEdge\src\adm2\custom\cntnpropcustom.i    x  P  %C:\Progress\OpenEdge\src\adm2\custom\cntnprtocustom.i    �  F>  C:\Progress\OpenEdge\src\adm2\visprop.i     �I  %C:\Progress\OpenEdge\src\adm2\custom\vispropcustom.i 4  ��  %C:\Progress\OpenEdge\src\adm2\custom\visprtocustom.i t  �l 
 C:\Progress\OpenEdge\src\adm2\appsprop.i �  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    �  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    ,  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i p  �j  C:\Progress\OpenEdge\gui\get �  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    �  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i       ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i T   Su  C:\Progress\OpenEdge\src\adm2\globals.i  �   M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i �   )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   �   �  C:\Progress\OpenEdge\src\adm2\appsprto.i @!  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   t!  �X 	 C:\Progress\OpenEdge\src\adm2\visprto.i  �!  !�  %C:\Progress\OpenEdge\src\adm2\custom\visualdefscustom.i  �!  n�  C:\Progress\OpenEdge\src\adm2\cntnprto.i 4"  ;  %C:\Progress\OpenEdge\src\adm2\custom\containrdefscustom.i    h"  ~�  C:\Progress\OpenEdge\src\adm2\widgetprto.i   �"  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   �"  �2   d:\newsie\on_in_co\APLIC\cbd\sunat-diario-copia.w                 `#     �  $   p#  �   �      �#  �   �     �#     �     �#  �        �#     ]     �#  �   U     �#     �  #   �#  �   �     �#     �       $  �   �     $     �       $  �   �     0$     �      @$  r   �     P$  n   �     `$     L  "   p$  i   G     �$     %     �$  P        �$  �        �$     �  !   �$  �   �     �$     �     �$  �   �     �$     a      %  �   _     %     =      %  g   #     0%          @%  O   �     P%  �   v     `%     t      p%  �   D     �%     �     �%  �   �     �%     �     �%  �   �     �%     �     �%  �   �     �%     y     �%  �   x      &     V     &  �   E      &     #     0&  �         @&     �     P&  }   �     `&     �     p&     T     �&          �&     �     �&  7   |     �&  �   s     �&  O   e     �&     T     �&          �&  �   �
      '  �   �
     '  O   �
      '     �
     0'     H
     @'  �   #
     P'  x   
  
   `'  M   
     p'     �	     �'     �	     �'  a   �	  
   �'  �  q	     �'     R	     �'  �  	     �'  O   	     �'      	     �'     �      (  �   �     (     �      (          0(  x   �     @(     �     P(     m     `(     i     p(     U     �(     <     �(  Q   ,  
   �(     �     �(     �  
   �(     �     �(     l  
   �(  f   A     �(     �  	    )  "   �     )     �      )     g     0)  Z        @)          P)     �     `)     �     p)     �     �)     {     �)  )   �       �)     B      �)            �)           