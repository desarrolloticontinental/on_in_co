	��V�:�ax6  ��              �                                �� 36780113utf-8 MAIN d:\newsie\on_in_co\APLIC\vta2\w-consolidado-cot-x-atender.w,,INPUT p-Division CHARACTER PROCEDURE um-saldos-almacen,,INPUT p-almacen CHARACTER,INPUT p-codmat CHARACTER,INPUT p-fecha DATE,OUTPUT p-Stock DECIMAL PROCEDURE um-procesar,, PROCEDURE um-excel,, PROCEDURE recoge-parametros,, PROCEDURE procesa-parametros,, PROCEDURE initializeObject,, PROCEDURE exitObject,, PROCEDURE enable_UI,, PROCEDURE disable_UI,, PROCEDURE adm-create-objects,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE _corre-program,, PROCEDURE _busca-lookup,,INPUT campo_name CHARACTER,INPUT program_call CHARACTER,OUTPUT program_name CHARACTER PROCEDURE adm-imprime,, PROCEDURE adm-busca,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE startServerObject,, PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE restartServerObject,, PROCEDURE initializeServerObject,, PROCEDURE disconnectObject,, PROCEDURE destroyServerObject,, PROCEDURE bindServer,, PROCEDURE processAction,,INPUT pcAction CHARACTER PROCEDURE enableObject,, PROCEDURE disableObject,, PROCEDURE applyLayout,, PROCEDURE viewPage,,INPUT piPageNum INTEGER PROCEDURE viewObject,, PROCEDURE toolbar,,INPUT pcValue CHARACTER PROCEDURE selectPage,,INPUT piPageNum INTEGER PROCEDURE removePageNTarget,,INPUT phTarget HANDLE,INPUT piPage INTEGER PROCEDURE passThrough,,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER PROCEDURE notifyPage,,INPUT pcProc CHARACTER PROCEDURE initPages,,INPUT pcPageList CHARACTER PROCEDURE initializeVisualContainer,, PROCEDURE hidePage,,INPUT piPageNum INTEGER PROCEDURE destroyObject,, PROCEDURE deletePage,,INPUT piPageNum INTEGER PROCEDURE createObjects,, PROCEDURE constructObject,,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE PROCEDURE confirmExit,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE changePage,, PROCEDURE assignPageProperty,,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION createUiEvents,LOGICAL, FUNCTION getObjectSecured,LOGICAL, FUNCTION getObjectTranslated,LOGICAL, FUNCTION setResizeVertical,LOGICAL,INPUT plResizeVertical LOGICAL FUNCTION setResizeHorizontal,LOGICAL,INPUT plResizeHorizontal LOGICAL FUNCTION setObjectLayout,LOGICAL,INPUT pcLayout CHARACTER FUNCTION setLayoutOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setHideOnInit,LOGICAL,INPUT plHide LOGICAL FUNCTION setDisableOnInit,LOGICAL,INPUT plDisable LOGICAL FUNCTION setDefaultLayout,LOGICAL,INPUT pcDefault CHARACTER FUNCTION setAllFieldNames,LOGICAL,INPUT pcValue CHARACTER FUNCTION setAllFieldHandles,LOGICAL,INPUT pcValue CHARACTER FUNCTION getResizeVertical,LOGICAL, FUNCTION getResizeHorizontal,LOGICAL, FUNCTION getWidth,DECIMAL, FUNCTION getRow,DECIMAL, FUNCTION getObjectLayout,CHARACTER, FUNCTION getObjectEnabled,LOGICAL, FUNCTION getLayoutVariable,CHARACTER, FUNCTION getLayoutOptions,CHARACTER, FUNCTION getHideOnInit,LOGICAL, FUNCTION getHeight,DECIMAL, FUNCTION getEnabledObjHdls,CHARACTER, FUNCTION getEnabledObjFlds,CHARACTER, FUNCTION getDisableOnInit,LOGICAL, FUNCTION getDefaultLayout,CHARACTER, FUNCTION getCol,DECIMAL, FUNCTION getAllFieldNames,CHARACTER, FUNCTION getAllFieldHandles,CHARACTER, FUNCTION setStatusArea,LOGICAL,INPUT plStatusArea LOGICAL FUNCTION getObjectType,character, FUNCTION setWindowTitleViewer,LOGICAL,INPUT phViewer HANDLE FUNCTION setWaitForObject,LOGICAL,INPUT phObject HANDLE FUNCTION setUpdateTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setUpdateSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setTopOnly,LOGICAL,INPUT plTopOnly LOGICAL FUNCTION setSdoForeignFields,LOGICAL,INPUT cSdoForeignFields CHARACTER FUNCTION setSavedContainerMode,LOGICAL,INPUT cSavedContainerMode CHARACTER FUNCTION setRunMultiple,LOGICAL,INPUT plMultiple LOGICAL FUNCTION setRunDOOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setRouterTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setReEnableDataLinks,LOGICAL,INPUT cReEnableDataLinks CHARACTER FUNCTION setPrimarySdoTarget,LOGICAL,INPUT hPrimarySdoTarget HANDLE FUNCTION setPageSource,LOGICAL,INPUT phObject HANDLE FUNCTION setPageNTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setOutMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setNavigationTarget,LOGICAL,INPUT cTarget CHARACTER FUNCTION setNavigationSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setNavigationSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setMultiInstanceSupported,LOGICAL,INPUT lMultiInstanceSupported LOGICAL FUNCTION setMultiInstanceActivated,LOGICAL,INPUT lMultiInstanceActivated LOGICAL FUNCTION setInMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setFilterSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDynamicSDOProcedure,LOGICAL,INPUT pcProc CHARACTER FUNCTION setDisabledAddModeTabs,LOGICAL,INPUT cDisabledAddModeTabs CHARACTER FUNCTION setCurrentPage,LOGICAL,INPUT iPage INTEGER FUNCTION setContainerTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setContainerMode,LOGICAL,INPUT cContainerMode CHARACTER FUNCTION setCallerWindow,LOGICAL,INPUT h HANDLE FUNCTION setCallerProcedure,LOGICAL,INPUT h HANDLE FUNCTION setCallerObject,LOGICAL,INPUT h HANDLE FUNCTION pageNTargets,CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER FUNCTION getStatusArea,LOGICAL, FUNCTION getWindowTitleViewer,HANDLE, FUNCTION getWaitForObject,HANDLE, FUNCTION getUpdateTarget,CHARACTER, FUNCTION getUpdateSource,CHARACTER, FUNCTION getTopOnly,LOGICAL, FUNCTION getSdoForeignFields,CHARACTER, FUNCTION getSavedContainerMode,CHARACTER, FUNCTION getRunMultiple,LOGICAL, FUNCTION getRunDOOptions,CHARACTER, FUNCTION getReEnableDataLinks,CHARACTER, FUNCTION getPrimarySdoTarget,HANDLE, FUNCTION getPageSource,HANDLE, FUNCTION getPageNTarget,CHARACTER, FUNCTION getOutMessageTarget,HANDLE, FUNCTION getNavigationTarget,HANDLE, FUNCTION getNavigationSourceEvents,CHARACTER, FUNCTION getNavigationSource,CHARACTER, FUNCTION getMultiInstanceSupported,LOGICAL, FUNCTION getMultiInstanceActivated,LOGICAL, FUNCTION getFilterSource,HANDLE, FUNCTION getDynamicSDOProcedure,CHARACTER, FUNCTION getDisabledAddModeTabs,CHARACTER, FUNCTION getCurrentPage,INTEGER, FUNCTION getContainerTargetEvents,CHARACTER, FUNCTION getContainerTarget,CHARACTER, FUNCTION getContainerMode,CHARACTER, FUNCTION getCallerWindow,HANDLE, FUNCTION getCallerProcedure,HANDLE, FUNCTION enablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION disablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION widgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION widgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION widgetIsTrue,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsModified,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetIsFocused,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsBlank,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetLongcharValue,LONGCHAR,INPUT pcName CHARACTER FUNCTION widgetHandle,HANDLE,INPUT pcName CHARACTER FUNCTION viewWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION toggleWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION resetWidgetValue,LOGICAL,INPUT pcNameList CHARACTER FUNCTION highlightWidget,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER FUNCTION hideWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION formattedWidgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION formattedWidgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION enableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION enableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION disableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION disableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION clearWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION blankWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION assignWidgetValueList,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION assignWidgetValue,LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER FUNCTION assignFocusedWidget,LOGICAL,INPUT pcName CHARACTER      dS              �C             �p dS  T�              `�              D4    +   � `     x� `     �� �  	   �� l  
   0� �  A   �� `  B   0� �   M   $� |  N   �� `  O    � $  P   $� �  Q        R   @    S   ` l  T   �!   U   �@   V           �D �  ? xJ <&  iSO8859-1                                                                           HR   ) �           �                         �              �  P�                    �E     �E   ��   $�  �R          �  �   ,S      8S                                                        PROGRESS                         \           
    
                    �              �                                                                                                     
  �                      �         @             ,                                                                                          �             p         �       �  L  �Q  (   �Q  �  9l       R         �             H          �I      �   �  n            �  z            �  �            �  �              �                �  	                       INTEGRAL                         PROGRESS                         �     �         �                         �ɺ[            �  b|                              �  �                      ,     ,      PROGRAMA_LOOKUPPROGRAMA_CALLCAMPODESCRIPCION                                          �      �  
    
                  p  8             �                                                                                          �          
  �  �      0  
    
                    �  	           �                                                                                          �          
  `  �      �  
    
                  �  �  
           L                                                                                          �          
    �      �  
    
                  t  <             �                                                                                          �          
  �        4  
    
                     �             �                                                                                                    
  d        �  
    
                  �  �             P                                                                                                    
  	  (      �  
    
                  x  @	             �                                                                                          (          
  �	  >      8	  
    
                  $	  �	             �	                                                                                          >          
  h
  L      �	                         �	  �
             T
                                                                                          L              Y      �
                        |
  D                                                                                                        Y            �  g      <  
    
                  (  �             �                                                                                          g          
  l  u      �  
    
                  �  �             X                                                                                          u          
    �      �  
    
                  �  H                                                                                                       �          
  �  �      @                        ,  �             �                                                                                          �            p  �      �                        �  �             \                                                                                          �              �      �                        �  L                                                                                                       �                �      D                        0  �             �                                                                                          �            �     0!         0!                         �\            8!  '�                              �  H                      �  X  RT     CODCLINOMCLIDIRCLIRUCTPOCLILOCCLIFAXCLIFCHINGFLGSITCODPOSCODCIAREFERENCIASCODPAISCODDEPTCODPROVCODDISTGIRCLICNDVTATELFNOSE-MAILFCHACTCLFCLIUSUARIOREPLEGFNREPRTRANSPORTEMONLCIMPLCCODVENDIRENTFCHCESCONTACFCHVLCCODANTUSRLCVENANTFLAGAUTUSRAUTOBSAUTCODDIVDEPENTPROENTDISENTLINCREUSRCRETOTUSOFCHAUTAVAL1AVAL2TLFLOGJFELOGDNIRUCOLDCANALCLFCOMNROCARDDIRREFCODUNICOAPEPATAPEMATNOMBRECOB_DIASCOB_HORARIOCOB_DIRECCIONCOB_CARTACOB_GLOSACODIBCCLFCLI2LIBRE_C01LIBRE_C02LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02LIBRE_L01LIBRE_L02FLAGTIPOFLAGUSUARIOFLAGMIGRACIONFLAGFECHACM_CLFCLI_PCM_CLFCLI_TSWBAJASUNATSWCARGASUNAT                                                                        	          
                                                                                                                                                                                                                                    !          "          #          $          %          &          '          (          )          *          +          ,          -         .         /          0         1         2         3          4         5          6          7          8          9          :          ;          <          =          >          ?          @          A          B          C          D          E          F          G          H          I          J          K          L          M          N          O          Q          R          S          T          U          V          t     �!         �!                         �M�]            �!  ~                              �                          ,  �3     CODDIVDESDIVCODCIADIRDIVTELDIVFAXDIVRESPONFLGREPFLGPREUNIFLGAPRCOTFLGAPRPEDFLGPREVTADIASVTOCOTDIASVTOPEDDIASVTOO_DDIASAMPCOTTIPDTOMODPREUNIFLGEMPAQUEFLGROTACIONCANALVENTAFLGPICKINGFLGBARRASFLGMINVENTAFLGDTOPROMFLGDTOVOLFLGTARJETAVENTAMOSTRADORVENTAMAYORISTAVENTAMINORISTAFLGDTOCLFCLIPORDTOCLFCLIFLGDTOCNDVTALIBRE_C01LIBRE_C02LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02LIBRE_L01LIBRE_L02FLAGTIPOFLAGUSUARIOFLAGMIGRACIONFLAGFECHACAMPO-LOGCAMPO-DECCAMPO-DATECAMPO-CHARGRUPO_DIVI_GGCENTRO_COSTO                                                                       	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          '          (          )          *          +          ,          .          /          0 
        1 
        2 
        3 
        4          5          `)  !   �$         �$                         ��{a            �$  ��                              �  �                      ("    #�     CODCIACODDOCNROPEDFCHPEDCODCLINOMCLIDIRCLIRUCCLIHORAFLGESTCODALMCODMONTPOCMBUSUARIOGLOSAIMPBRTIMPEXOPORIGVIMPDTOIMPTOTIMPIGVCODVENIMPISCIMPVTAFLGSITFMAPGOFCHVENORDCMPCODDIVTIPVTAPORDTOTPOPEDUSRDSCTOLUGENTCODTRANSNROREFCMPBNTEOBSERVANCMPBNTEATENCIONFAXCLILUGENT2IMPFLEFLGIGVTPOLICUBIGEOACUBONNROCARDTIPBONIMPORTEPORCENTUSRAPROBACIONFCHAPROBACIONFCHENTCODPOSFLGENVUSRSACFECSACHORSACLIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02USRCHQFCHCHQHORCHQSEDEDIVDESCODREFIMPDTO2GLOSAIMPDTO2FLGIMPODUSRIMPODFCHIMPODUSRACTFECACTHORACTMOTREPOSICIONVTAPUNTUALCROSSDOCKINGALMACENXDCODORIGENNROORIGENDTALMACENDTEMPAQESPECCLIENTE_RECOGELISTA_DE_PRECIOSCUSTOMERPURCHASEORDERCUSTOMERREQUESTORDERTYPEPERIODCURRENCYPAYFORMDELIVERYDATEREGION1REGION1NAMEREGION2REGION2NAMEREGION3REGION3NAMETELEPHONECONTACTRECEPTORCONTACTRECEPTORNAMEDELIVERYADDRESSCUSTOMERLABELOFFICECUSTOMEROFFICECUSTOMERNAMECUSTOMERSTOCKDEPOCUSTOMERSTOCKDEPONAMECONSOLIDATEINVOICECUSTOMERINVOICECUSTOMERGROUPITEMSPESOVOLUMENEMBALAJE_ESPECIALDELIVERYGROUPDNICLIE-MAILIMPPERCEPCIONPORPERCEPCIONCODPAISCODDEPTCODPROVCODDISTREFERENCEADDRESSIDCUSTOMERFLAGMIGRACIONMIGFECHAMIGHORAMIGUSUARIOTOTALVALORVENTANETOOPGRAVADASTOTALVALORVENTANETOOPGRATUITASTOTALTRIBUTOSOPEGRATUITASTOTALIGVTOTALIMPUESTOSTOTALVALORVENTATOTALPRECIOVENTADESCUENTOSGLOBALESPORCENTAJEDSCTOGLOBALMONTOBASEDESCUENTOGLOBALTOTALVALORVENTANETOOPNOGRAVADATOTALDOCUMENTOANTICIPOMONTOBASEDSCTOGLOBALANTICIPOPORCENTAJEDSCTOGLOBALANTICIPOTOTALDSCTOGLOBALESANTICIPOMONTOBASEICBPERTOTALMONTOICBPERTOTALVALORVENTANETOOPEXONERADASTOTALVENTA                                                                       	          
                                                                                                                                                                                                                                       !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /         0 
        1          2 
        3         4         5          6          7          8          9          :          ;          <          =          >          ?          @          A          B          C          D          E          F          G          H          I          J          K          L          M          N          O          P   "       Q          R          S          T          U          V          W          X          Y          Z          [          \          ]          ^          _          `          a          b          d          e          f          g          h          i          j          k          l          m          n          o          p          q          r          s          t          u          v          w          x          y          z          {          |          }          ~                    �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �          �2  "   �$         �$                         Y|a            �$  M                              �  �)                      (.  �)  7_     CODDOCCODCIAFACTORUNDVTACODMATNROPEDCANPEDPREUNIPORDTOIMPDTOIMPLINNROITMAFTIGVAFTISCPREBASPREVTAIMPIGVIMPISCCANATEHORAFLGESTFCHPEDCODAUXMRGUTIPORDTO2TIPVTAPESMATCODCLIALMDESPOR_DSCTOSFLG_FACTORCODDIVCANPICKLIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02CANSOLCANAPRIMPDTO2ALMTRFCANTRFLIBRE_D03LIBRE_D04LIBRE_D05LIBRE_F05LIBRE_F03LIBRE_F04SDOCOTCODMATWEBDESMATWEBCANPEDWEBPREUNIWEBIMPLINWEBCUSTOMERARTCODECUSTOMERARTDESCRIPTIONCUSTOMERUNITCODECUSTOMERUNITCODENAMEQTYPRICEROWTOTALCUSTOMERCURRENCYCUSTOMEROLDARTCODEFACTORDESCUENTOTASAIGVIMPORTEUNITARIOSINIMPUESTOIMPORTEREFERENCIALIMPORTEBASEDESCUENTOIMPORTEDESCUENTOIMPORTETOTALSINIMPUESTOMONTOBASEIGVIMPORTEIGVIMPORTETOTALIMPUESTOSIMPORTEUNITARIOCONIMPUESTOCIMPORTEVENTAEXONERADOCIMPORTEVENTAGRATUITOCOTROSTRIBUTOSOPGRATUITOCTIPOAFECTACIONCPREUNISINIMPUESTOCSUMAIMPTETOTALSINIMPUESTOCMONTOBASEIGVCSUMAIGVIMPUESTOBOLSAPLASTICOMONTOTRIBUTOBOLSAPLASTICOCANTIDADBOLSAPLASTICOMONTOUNITARIOBOLSAPLASTICOCIMPORTETOTALCONIMPUESTOIMPORTEBASEDESCUENTONOAFECTOFACTORDESCUENTONOAFECTOIMPORTEDESCUENTONOAFECTO                                                                       	          
                                                                                                                                                                                                                                     !          "          #          $          %          &          '          (          )          *          +          ,          -          .          /          0          1          2          3          4          5          6          7          8          9          :          ;          <          =          >          ?          @          A          B          C          D          E          S          T          U          V          W          X          Y          Z          [          \          ]          ^          _          `          a          b          c          d          e          f          g          h          i          j          k          l          m          @  #   �$         �$                         �#sa            �$  �                              �  3                      |8  ,3  P�     CODMATDESMATCODMARUNDSTKUNDCMPFACEQUCODCTACODNEWMONVTAPREVTAPREBASAFTIGVVINMN1CODCIAVINMN2CODFAMVCTMN1FCHACTCODPR1CODPR2VCTMN2ARTPROFCHUSALFCHUCMPPMAXMN1PMAXMN2PULTMN1PULTMN2USUARIOFCHINGFCHCESFCHALZCLFMATUNDBASSUBFAMCODBRRCODANTTIPARTFCHPRMDFCHPRMHFCHREAPESMATDETALLECANEMPALMACENESDESMARAFTISCPORISCPORVTATPOMRGCTOLISCTOPRMMRGUTIPORMAXFCHMPREUNDANTPREANTPREACTDSCTOSTPOPROPORIGVCTOTOTTPOSUMCTOUNDORDENORDLISORDTMPTPOARTTPOCMBPPCHR__01CHR__02CHR__03DEC__01DEC__02DEC__03DATE__01DATE__02DATE__03MRGUTI-AMRGUTI-BMRGUTI-CPREOFIUNDAUNDBUNDCFLGINTFLGPRECLASEFCHPROMCATCONTATIPROTDSCTOPROMINFORFLGINFORPROMDIVIPROMFCHDPROMFCHHPROMDTODTOVOLRDTOVOLDUNDALTDSCALTMRGALTPREALTLICENCIAPROMMINDIVIPROMMINFCHDPROMMINFCHHPROMMINDTOCODDIGESAVTODIGESALIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02STKMINSTKMAXSTKREPDESCRIPCION-LARGADESCRIPCION-TECNICASW-WEBWEB-SUBCATEGORIALIBRE_D03LIBRE_D04LIBRE_D05PESOBRUTOPAQUETELARGOALTOANCHOCTOLISMARCOCTOTOTMARCOCODSSFAMCLFESPECIALFLGCOMERCIALLIBRE_C06LIBRE_C07LIBRE_C08LIBRE_C09LIBRE_C10CODIGOPADREFACTORPADREREQUIERESERIALNRREQUIEREDUEDATEDTOVOLPTASAIMPUESTOIMPORTEUNITARIOSINIMPUESTODTOVOLPSINIMPUESTOIMPORTEUNITARIOSINIMPUESTO_AIMPORTEUNITARIOSINIMPUESTO_BIMPORTEUNITARIOSINIMPUESTO_CDTOVOLPIMPUESTOIMPORTEUNITARIOIMPUESTOIMPORTEUNITARIOIMPUESTO_AIMPORTEUNITARIOIMPUESTO_BIMPORTEUNITARIOIMPUESTO_C                                                                      	          
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
        �          �          �          �          �A  $   �$         �$                         �ɺ[            %  im                              �  �@                      A  �@  t      CODFAMDESFAMCODCIATPOCMBLIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02SWCOMERCIAL                                                                      	          
                                                            |C  %   %         %                         �ɺ[             %  8�                              �  @B                      �B  PB  v      CODCIASUBFAMDESSUBCODFAMORDENSWDIGESALIBRE_C01LIBRE_C02LIBRE_C03LIBRE_C04LIBRE_C05LIBRE_D01LIBRE_D02LIBRE_F01LIBRE_F02                                                                        	          
                                                                      |D  &   5%         5%                         �ɺ[            >%  �e                              �  �C                      4D  D  %      TABLACODIGONOMBRENOMANTCODCTA1CODCTA2                                                                   (   �%         �%                         �ɺ[            �%  )
                              �  �D                      8E  E  )      CODCIACODALMUNDVTASTKACTCTOUNIFECHACODMAT                                                                                       ��                                              2 ��          dG  �G  \ ��E                          
                                           < Todos los Clientes >                                                          
             
             
                                         
                                                                                                                \   l   |   �   �   �   �   �   �   �       ,  <  L  \  l  |  �  �  �  �      \   l   |   �   �   �   �   �   �   �      ,  <  L  \  l  |  �  �  �  �                                                                                                                                     	                  
                                                                                                                                                                                                   �L  �L  �L  M   M                        M   M  (M  @M  4M          DM             `M  lM  tM  �M  |M          �M             �M  �M  �M  �M                              �M  �M  �M  �M  �M                         �M  N  N  N                              N  (N  0N  @N  8N                         DN  PN  XN  dN                              hN  tN  |N  �N  �N          �N             �N  �N  �N  �N  �N          �N             O  O  ,O  LO  <O          PO             `O  lO  �O  �O  �O          �O             �O  �O  �O  �O  �O          �O             P  P  (P  HP  8P          LP             \P  hP  |P  �P  �P          �P             �P  �P  �P  �P  �P          �P             Q  Q  $Q  DQ  4Q          HQ             XQ  dQ  xQ  �Q  �Q          �Q                                                         tt-codmat   X(6)    Codigo Articulo Codigo Articulo     tt-DesMat   X(45)   Descripci�n Descripci�n     Descripci�n del material    tt-CodFam   X(3)    Familia C�digo!familia      Codigo de familia   tt-desFam   X(80)   tt-desFam       tt-subFam   X(3)    Sub-Familia Sub!Familia     tt-dessub   X(80)   tt-dessub       tt-codmar   X(4)    Marca   Marca       tt-desmar   X(80)   tt-desmar       tt-undstk   X(8)    Unidad de stock Unidad!stock        Unidad de stock tt-qty-pedida   Z,ZZZ,ZZZ,ZZ9.99    Stock maximo    Stock!maximo    0   Stock maximo    tt-qty-atendida Z,ZZZ,ZZZ,ZZ9.99    Stock maximo    Stock!maximo    0   Stock maximo    tt-xatender Z,ZZZ,ZZZ,ZZ9.99    Stock maximo    Stock!maximo    0   Stock maximo    tt-stk-11s  Z,ZZZ,ZZZ,ZZ9.99    Stock maximo    Stock!maximo    0   Stock maximo    tt-stk-21s  Z,ZZZ,ZZZ,ZZ9.99    Stock maximo    Stock!maximo    0   Stock maximo    tt-stk-11   Z,ZZZ,ZZZ,ZZ9.99    Stock maximo    Stock!maximo    0   Stock maximo    tt-stk-35   Z,ZZZ,ZZZ,ZZ9.99    Stock maximo    Stock!maximo    0   Stock maximo    tt-stk-21   Z,ZZZ,ZZZ,ZZ9.99    Stock maximo    Stock!maximo    0   Stock maximo    tt-stk-38   Z,ZZZ,ZZZ,ZZ9.99    Stock maximo    Stock!maximo    0   Stock maximo    �  �  ���������                      �%                �     i     	    �  �  �  �  �  �  �  �        *  6  A  L  V  `  j    ��                                               �          ����                            &   ��    �%   �    &   ��    �%         &  ! ��    &  " �     &  # ��    '&  $ �'    .&  % [    5&  & �y    &  (  {    undefined                                                               �       ��  �   l   �    �                  �����               ��a                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D       p
     7          assignFocusedWidget         �      �            LOGICAL,INPUT pcName CHARACTER  assignWidgetValue   �      �      $    $       LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER  assignWidgetValueList         \      �    6       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER  blankWidget t      �          L       LOGICAL,INPUT pcNameList CHARACTER  clearWidget �      @      l    X       LOGICAL,INPUT pcNameList CHARACTER  disableRadioButton  L      �      �    d       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    disableWidget   �            4    w       LOGICAL,INPUT pcNameList CHARACTER  enableRadioButton         X      �    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    enableWidget    l      �      �    �       LOGICAL,INPUT pcNameList CHARACTER  formattedWidgetValue    �             X  	  �       CHARACTER,INPUT pcName CHARACTER    formattedWidgetValueList    8      |      �  
  �       CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    hideWidget  �      �      (   
 �       LOGICAL,INPUT pcNameList CHARACTER  highlightWidget       L      |    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER  resetWidgetValue    \      �      �    �       LOGICAL,INPUT pcNameList CHARACTER  toggleWidget    �            H    �       LOGICAL,INPUT pcNameList CHARACTER  viewWidget  (      l      �   
       LOGICAL,INPUT pcNameList CHARACTER  widgetHandle    x      �      �          HANDLE,INPUT pcName CHARACTER   widgetLongcharValue �            @    #      LONGCHAR,INPUT pcName CHARACTER widgetIsBlank          `      �    7      LOGICAL,INPUT pcNameList CHARACTER  widgetIsFocused p      �      �    E      LOGICAL,INPUT pcName CHARACTER  widgetIsModified    �      	      8	    U      LOGICAL,INPUT pcNameList CHARACTER  widgetIsTrue    	      \	      �	    f      LOGICAL,INPUT pcName CHARACTER  widgetValue l	      �	      �	    s      CHARACTER,INPUT pcName CHARACTER    widgetValueList �	      �	      ,
          CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    �    g  �
  �
  P  �       4   �����       o   h       �
                              �  �   NA  �   �  �   �       $     8    L    `    t    �    �  `  �  
`  �  $  �    �            $  y  |  ���                            
                    � ߱                                 � ߱           $  �  �  ���                       �  o   �      4      �                         4     H  �  \  �  p  �G  �  �  �     �     �                  (          �  �      ��                  �  �                �d                    O   ����    e�          O   ����    R�          O   ����    ��      d  /   �  T                                 3   �����        �  �     �    ��                            ����                                        t                    |                      g                                               �          �  �      ��                  �  �  �              �b                    O   ����    e�          O   ����    R�          O   ����    ��            �           ��                            ����                                                            �                      g                                 $       "�                  4                         � ߱        �  $  �  �  ���                         g              � �                            $          �  �      ��                      �              b                    O   ����    e�          O   ����    R�          O   ����    ��      t  @         `          �  @         �              � ߱            $     �  ���                         ��                              ��        �                  ����                                        (                    P                      g                               H  g     $          �4�                           �          �  �      ��                   !  �              �b                    O   ����    e�          O   ����    R�          O   ����    ��      0              �      4   �����      O     ��  ��             L  �            4   ����                �                      ��                                       �b                         \  t  /                                    3   ����<  D        4                      3   ����X            d                      3   ����l          x  }        ��                              ��        �                  ����                                        8                    �                      g                               �  g   #  `         ��            �4�                           <            �      ��                 #  '  $              T�b                    O   ����    e�          O   ����    R�          O   ����    ��            $  X  �      �      4   �����                �                      ��                  $  &                  �b                       $  h        %           �      4   �����        %  �     �    ��                              ��        �                  ����                                        �                    (                      g                               8"  g   7  �         �!4                            �          �  |      ��                 7  9  �              Dd                    O   ����    e�          O   ����    R�          O   ����    ��          8  �  �             4   ����       O   8  ��  ��  D        8  $  �  $  X      4   ����X                �                      ��                  8  8                  �Dd                       8  4  �     
  
       
       �     
                    � ߱        $  $  8  �  ���                       H  /   8  P     `                          3   �����  �        �                      3   �����  �        �                      3   �����            �  �                  3   �����      $   8    ���                                                   � ߱        l    8  d  �      �      4   �����                8                      ��                  8  8                   Ed                       8  t  0  @                   d  @         P              � ߱        d  $   8  �  ���                           p   8  �  �  T  8  �  �     �  �  �                         � ߱            $  8  �  ���                           (     �  �                         � ߱            $  8  �  ���                           O   8  ��  ��  �        8  �  �  �  �      4   �����  <  @         (              � ߱            $   8  �  ���                       p  @         \          �  @         �          �  @         �          (  @                   \  @         H              � ߱            $   8  �  ���                                     4                      ��                  8  8                  �Ed                       8  �        8  P  �      p      4   ����p  �  @         �           	  @         	              � ߱            $   8  `  ���                         ��                              ��        �                  ����                                                            �                      g                               adm-busca       �                                                            �  	                   adm-imprime �   �                                                            �                     _busca-lookup   !  `!  �       h         	     �                          �  �                     _corre-program  p!  �!              �     
     ,                          (                       h�    �  T"  �"      t      4   ����t                �"                      ��                  �  �                  �vc                       �  d"  d#    �  �"  #      �      4   �����      $  �  8#  ���                       �  @         �              � ߱              �  �#  �#      @      4   ����@      $  �  �#  ���                       �  @         |              � ߱        assignPageProperty                              �$  h$      ��                  U  X  �$              �Xc                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �$             �$               ��                  �$           ��                            ����                            changePage                              �%  �%      ��                  Z  [  �%              t�a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmExit                             �&  �&      ��                  ]  _  �&              @Va                    O   ����    e�          O   ����    R�          O   ����    ��            ��                   '           ��                            ����                            constructObject                             �'  �'      ��                  a  f  (              �hb                    O   ����    e�          O   ����    R�          O   ����    ��            ��   `(             ,(               �� 
  �(             T(  
             ��   �(             |(               �� 
                 �(  
         ��                            ����                            createObjects                               �)  �)      ��                  h  i  �)               �a                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deletePage                              �*  �*      ��                  k  m  �*              H�c                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �*           ��                            ����                            destroyObject                               �+  �+      ��                  o  p  �+              �Nc                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hidePage                                �,  �,      ��                  r  t  �,              |Oc                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �,           ��                            ����                            initializeObject                                �-  �-      ��                  v  w  .              �d                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeVisualContainer                               /  �.      ��                  y  z  $/              ��d                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initPages                               0  �/      ��                  |  ~  $0              8�d                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  <0           ��                            ����                            notifyPage                              41  1      ��                  �  �  L1              �a                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  d1           ��                            ����                            passThrough                             \2  D2      ��                  �  �  t2              �va                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �2             �2               ��                  �2           ��                            ����                            removePageNTarget                               �3  �3      ��                  �  �  �3              ��b                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  4             �3  
             ��                  4           ��                            ����                            selectPage                              5  �4      ��                  �  �  5              �pc                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  45           ��                            ����                            toolbar                             (6  6      ��                  �  �  @6              ��a                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  X6           ��                            ����                            viewObject                              P7  87      ��                  �  �  h7              �d                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewPage                                P8  88      ��                  �  �  h8              |vd                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �8           ��                            ����                            disablePagesInFolder    
      �8       9          LOGICAL,INPUT pcPageInformation CHARACTER   enablePagesInFolder  9      L9      �9    ,      LOGICAL,INPUT pcPageInformation CHARACTER   getCallerProcedure  `9      �9      �9    @      HANDLE, getCallerWindow �9      �9      :    S      HANDLE, getContainerMode    �9       :      T:    c      CHARACTER,  getContainerTarget  4:      `:      �:    t      CHARACTER,  getContainerTargetEvents    t:      �:      �:    �      CHARACTER,  getCurrentPage  �:      �:      ;    �      INTEGER,    getDisabledAddModeTabs  �:      $;      \;     �      CHARACTER,  getDynamicSDOProcedure  <;      h;      �;  !  �      CHARACTER,  getFilterSource �;      �;      �;  "  �      HANDLE, getMultiInstanceActivated   �;      �;       <  #  �      LOGICAL,    getMultiInstanceSupported    <      ,<      h<  $        LOGICAL,    getNavigationSource H<      t<      �<  %  !      CHARACTER,  getNavigationSourceEvents   �<      �<      �<  &  5      CHARACTER,  getNavigationTarget �<      �<      0=  '  O      HANDLE, getOutMessageTarget =      8=      l=  (  c      HANDLE, getPageNTarget  L=      t=      �=  )  w      CHARACTER,  getPageSource   �=      �=      �=  *  �      HANDLE, getPrimarySdoTarget �=      �=      >  +  �      HANDLE, getReEnableDataLinks    �=      $>      \>  ,  �      CHARACTER,  getRunDOOptions <>      h>      �>  -  �      CHARACTER,  getRunMultiple  x>      �>      �>  .  �      LOGICAL,    getSavedContainerMode   �>      �>      ?  /  �      CHARACTER,  getSdoForeignFields �>      $?      X?  0  �      CHARACTER,  getTopOnly  8?      d?      �?  1 
       LOGICAL,    getUpdateSource p?      �?      �?  2        CHARACTER,  getUpdateTarget �?      �?      @  3  !      CHARACTER,  getWaitForObject    �?      @      H@  4  1      HANDLE, getWindowTitleViewer    (@      P@      �@  5  B      HANDLE, getStatusArea   h@      �@      �@  6  W      LOGICAL,    pageNTargets    �@      �@      �@  7  e      CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER setCallerObject �@      4A      dA  8  r      LOGICAL,INPUT h HANDLE  setCallerProcedure  DA      |A      �A  9  �      LOGICAL,INPUT h HANDLE  setCallerWindow �A      �A      �A  :  �      LOGICAL,INPUT h HANDLE  setContainerMode    �A      B      DB  ;  �      LOGICAL,INPUT cContainerMode CHARACTER  setContainerTarget  $B      lB      �B  <  �      LOGICAL,INPUT pcObject CHARACTER    setCurrentPage  �B      �B      �B  =  �      LOGICAL,INPUT iPage INTEGER setDisabledAddModeTabs  �B      C      HC  >  �      LOGICAL,INPUT cDisabledAddModeTabs CHARACTER    setDynamicSDOProcedure  (C      xC      �C  ?  �      LOGICAL,INPUT pcProc CHARACTER  setFilterSource �C      �C       D  @        LOGICAL,INPUT phObject HANDLE   setInMessageTarget  �C       D      TD  A        LOGICAL,INPUT phObject HANDLE   setMultiInstanceActivated   4D      tD      �D  B  )      LOGICAL,INPUT lMultiInstanceActivated LOGICAL   setMultiInstanceSupported   �D      �D      E  C  C      LOGICAL,INPUT lMultiInstanceSupported LOGICAL   setNavigationSource �D      LE      �E  D  ]      LOGICAL,INPUT pcSource CHARACTER    setNavigationSourceEvents   `E      �E      �E  E  q      LOGICAL,INPUT pcEvents CHARACTER    setNavigationTarget �E      F      8F  F  �      LOGICAL,INPUT cTarget CHARACTER setOutMessageTarget F      XF      �F  G  �      LOGICAL,INPUT phObject HANDLE   setPageNTarget  lF      �F      �F  H  �      LOGICAL,INPUT pcObject CHARACTER    setPageSource   �F       G      0G  I  �      LOGICAL,INPUT phObject HANDLE   setPrimarySdoTarget G      PG      �G  J  �      LOGICAL,INPUT hPrimarySdoTarget HANDLE  setReEnableDataLinks    dG      �G      �G  K  �      LOGICAL,INPUT cReEnableDataLinks CHARACTER  setRouterTarget �G      H      @H  L  �      LOGICAL,INPUT phObject HANDLE   setRunDOOptions  H      `H      �H  M  		      LOGICAL,INPUT pcOptions CHARACTER   setRunMultiple  pH      �H      �H  N  	      LOGICAL,INPUT plMultiple LOGICAL    setSavedContainerMode   �H      I      @I  O  (	      LOGICAL,INPUT cSavedContainerMode CHARACTER setSdoForeignFields  I      lI      �I  P  >	      LOGICAL,INPUT cSdoForeignFields CHARACTER   setTopOnly  �I      �I      �I  Q 
 R	      LOGICAL,INPUT plTopOnly LOGICAL setUpdateSource �I      J      HJ  R  ]	      LOGICAL,INPUT pcSource CHARACTER    setUpdateTarget (J      lJ      �J  S  m	      LOGICAL,INPUT pcTarget CHARACTER    setWaitForObject    |J      �J      �J  T  }	      LOGICAL,INPUT phObject HANDLE   setWindowTitleViewer    �J      K      LK  U  �	      LOGICAL,INPUT phViewer HANDLE   getObjectType   ,K      lK      �K  V  �	      CHARACTER,  setStatusArea   |K      �K      �K  W  �	      LOGICAL,INPUT plStatusArea LOGICAL  applyLayout                             �L  tL      ��                      �L              H�b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableObject                               �M  xM      ��                      �M              ��b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            enableObject                                �N  |N      ��                      �N              ��`                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �O  �O      ��                     !  �O              ��`                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            processAction                               �P  �P      ��                  #  %  �P              H^c                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �P           ��                            ����                            getAllFieldHandles  �K      8Q      lQ  X  �	      CHARACTER,  getAllFieldNames    LQ      xQ      �Q  Y  �	      CHARACTER,  getCol  �Q      �Q      �Q  Z  �	      DECIMAL,    getDefaultLayout    �Q      �Q       R  [  �	      CHARACTER,  getDisableOnInit     R      ,R      `R  \  �	      LOGICAL,    getEnabledObjFlds   @R      lR      �R  ]  
      CHARACTER,  getEnabledObjHdls   �R      �R      �R  ^  
      CHARACTER,  getHeight   �R      �R      S  _ 	 0
      DECIMAL,    getHideOnInit   �R      $S      TS  `  :
      LOGICAL,    getLayoutOptions    4S      `S      �S  a  H
      CHARACTER,  getLayoutVariable   tS      �S      �S  b  Y
      CHARACTER,  getObjectEnabled    �S      �S      T  c  k
      LOGICAL,    getObjectLayout �S       T      PT  d  |
      CHARACTER,  getRow  0T      \T      �T  e  �
      DECIMAL,    getWidth    dT      �T      �T  f  �
      DECIMAL,    getResizeHorizontal �T      �T      �T  g  �
      LOGICAL,    getResizeVertical   �T      U      <U  h  �
      LOGICAL,    setAllFieldHandles  U      HU      |U  i  �
      LOGICAL,INPUT pcValue CHARACTER setAllFieldNames    \U      �U      �U  j  �
      LOGICAL,INPUT pcValue CHARACTER setDefaultLayout    �U      �U      $V  k  �
      LOGICAL,INPUT pcDefault CHARACTER   setDisableOnInit    V      HV      |V  l  �
      LOGICAL,INPUT plDisable LOGICAL setHideOnInit   \V      �V      �V  m        LOGICAL,INPUT plHide LOGICAL    setLayoutOptions    �V      �V       W  n        LOGICAL,INPUT pcOptions CHARACTER   setObjectLayout  W      DW      tW  o  '      LOGICAL,INPUT pcLayout CHARACTER    setResizeHorizontal TW      �W      �W  p  7      LOGICAL,INPUT plResizeHorizontal LOGICAL    setResizeVertical   �W      �W      ,X  q  K      LOGICAL,INPUT plResizeVertical LOGICAL  getObjectTranslated X      TX      �X  r  ]      LOGICAL,    getObjectSecured    hX      �X      �X  s  q      LOGICAL,    createUiEvents  �X      �X      Y  t  �      LOGICAL,    bindServer                              �Y  �Y      ��                      �Y              TSb                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �Z  �Z      ��                  
    �Z              �Sb                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �[  �[      ��                      �[              P/                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �\  �\      ��                      �\               0                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �]  �]      ��                      �]              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �^  �^      ��                      �^              <                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �_  �_      ��                      �_              �                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �_  
         ��                            ����                            startServerObject                               �`  �`      ��                      a              �0                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                 b  �a      ��                     "  b              �1                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  0b           ��                            ����                            getAppService   �X      �b      �b  u  �      CHARACTER,  getASBound  �b      �b       c  v 
 �      LOGICAL,    getAsDivision   �b      c      <c  w  �      CHARACTER,  getASHandle c      Hc      tc  x  �      HANDLE, getASHasStarted Tc      |c      �c  y  �      LOGICAL,    getASInfo   �c      �c      �c  z 	 �      CHARACTER,  getASInitializeOnRun    �c      �c      (d  {  �      LOGICAL,    getASUsePrompt  d      4d      dd  |  �      LOGICAL,    getServerFileName   Dd      pd      �d  }        CHARACTER,  getServerOperatingMode  �d      �d      �d  ~        CHARACTER,  runServerProcedure  �d      �d      (e    +      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   e      le      �e  �  >      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   |e      �e      �e  �  L      LOGICAL,INPUT pcDivision CHARACTER  setASHandle �e      f      Df  �  Z      LOGICAL,INPUT phASHandle HANDLE setASInfo   $f      df      �f  � 	 f      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    pf      �f      �f  �  p      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �f      g      <g  �  �      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   g      \g      �g  �  �      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  pg      �g      �g  �  �      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �h  �h      ��                  �  �  �h              T�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  i             �h  
             ��   4i              i               �� 
                 (i  
         ��                            ����                            addMessage                               j  j      ��                  �  �  8j              6                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �j             Pj               ��   �j             xj               ��                  �j           ��                            ����                            adjustTabOrder                              �k  �k      ��                  �  �  �k              4l                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
   l             �k  
             �� 
  (l             �k  
             ��                  l           ��                            ����                            applyEntry                              m  �l      ��                  �  �  ,m              ؇                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  Dm           ��                            ����                            changeCursor                                @n  (n      ��                  �  �  Xn              �]                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  pn           ��                            ����                            createControls                              lo  To      ��                  �     �o              \^                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               pp  Xp      ��                      �p              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                tq  \q      ��                      �q              h�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �r  hr      ��                    	  �r              H�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �s  hs      ��                      �s              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �t  ht      ��                      �t              P�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �u  pu      ��                      �u              Л                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              �v  xv      ��                      �v              М                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �v             �v  
             ��   w             �v               ��   Dw             w               ��                  8w           ��                            ����                            modifyUserLinks                             4x  x      ��                      Lx              �<                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �x             dx               ��   �x             �x               �� 
                 �x  
         ��                            ����                            removeAllLinks                              �y  �y      ��                  !  "  �y               �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              �z  �z      ��                  $  (  �z              �!                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  {             �z  
             ��   <{             {               �� 
                 0{  
         ��                            ����                            repositionObject                                0|  |      ��                  *  -  H|              �}                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �|             `|               ��                  �|           ��                            ����                            returnFocus                             �}  h}      ��                  /  1  �}              �~                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �}  
         ��                            ����                            showMessageProcedure                                �~  �~      ��                  3  6  �~              �\                    O   ����    e�          O   ����    R�          O   ����    ��            ��                �~               ��                             ��                            ����                            toggleData                              �  �      ��                  8  :  �              D�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  4�           ��                            ����                            viewObject                              ,�  �      ��                  <  =  D�              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �g      ��      ȁ  � 
       LOGICAL,    assignLinkProperty  ��      ԁ      �  �        LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   �      `�      ��  �  )      CHARACTER,  getChildDataKey p�      ��      ̂  �  7      CHARACTER,  getContainerHandle  ��      ؂      �  �  G      HANDLE, getContainerHidden  �      �      H�  �  Z      LOGICAL,    getContainerSource  (�      T�      ��  �  m      HANDLE, getContainerSourceEvents    h�      ��      ̃  �  �      CHARACTER,  getContainerType    ��      ؃      �  �  �      CHARACTER,  getDataLinksEnabled �      �      L�  �  �      LOGICAL,    getDataSource   ,�      X�      ��  �  �      HANDLE, getDataSourceEvents h�      ��      Ą  �  �      CHARACTER,  getDataSourceNames  ��      Є      �  �  �      CHARACTER,  getDataTarget   �      �      @�  �  �      CHARACTER,  getDataTargetEvents  �      L�      ��  �        CHARACTER,  getDBAware  `�      ��      ��  � 
       LOGICAL,    getDesignDataObject ��      ą      ��  �         CHARACTER,  getDynamicObject    ؅      �      8�  �  4      LOGICAL,    getInstanceProperties   �      D�      |�  �  E      CHARACTER,  getLogicalObjectName    \�      ��      ��  �  [      CHARACTER,  getLogicalVersion   ��      ̆       �  �  p      CHARACTER,  getObjectHidden ��      �      <�  �  �      LOGICAL,    getObjectInitialized    �      H�      ��  �  �      LOGICAL,    getObjectName   `�      ��      ��  �  �      CHARACTER,  getObjectPage   ��      ȇ      ��  �  �      INTEGER,    getObjectParent ؇      �      4�  �  �      HANDLE, getObjectVersion    �      <�      p�  �  �      CHARACTER,  getObjectVersionNumber  P�      |�      ��  �  �      CHARACTER,  getParentDataKey    ��      ��      �  �  �      CHARACTER,  getPassThroughLinks Ԉ       �      4�  �        CHARACTER,  getPhysicalObjectName   �      @�      x�  �         CHARACTER,  getPhysicalVersion  X�      ��      ��  �  6      CHARACTER,  getPropertyDialog   ��      ĉ      ��  �  I      CHARACTER,  getQueryObject  ؉      �      4�  �  [      LOGICAL,    getRunAttribute �      @�      p�  �  j      CHARACTER,  getSupportedLinks   P�      |�      ��  �  z      CHARACTER,  getTranslatableProperties   ��      ��      ��  �  �      CHARACTER,  getUIBMode  ؊      �      0�  � 
 �      CHARACTER,  getUserProperty �      <�      l�  �  �      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    L�      ��      ̋  �  �      CHARACTER,INPUT pcPropList CHARACTER    linkHandles ��      �       �  �  �      CHARACTER,INPUT pcLink CHARACTER    linkProperty     �      D�      t�  �  �      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry T�      ��      ܌  �  �      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   ��      H�      x�  �  �      CHARACTER,INPUT piMessage INTEGER   propertyType    X�      ��      ̍  �  	      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  ��      �      $�  �        CHARACTER,  setChildDataKey �      0�      `�  �  %      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  @�      ��      ��  �  5      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  ��      ܎      �  �  H      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    ��      0�      l�  �  [      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled L�      ��      ď  �  t      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   ��      �      �  �  �      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents ��      <�      p�  �  �      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  P�      ��      ̐  �  �      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   ��      ��      $�  �  �      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents �      H�      |�  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  \�      ��      ̑  � 
 �      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject ��      �       �  �  �      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject     �      H�      |�  �  �      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   \�      ��      В  �        LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    ��      ��      ,�  �  %      LOGICAL,INPUT c CHARACTER   setLogicalVersion   �      H�      |�  �  :      LOGICAL,INPUT cVersion CHARACTER    setObjectName   \�      ��      Г  �  L      LOGICAL,INPUT pcName CHARACTER  setObjectParent ��      �       �  �  Z      LOGICAL,INPUT phParent HANDLE   setObjectVersion     �      @�      t�  �  j      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    T�      ��      Д  �  {      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks ��      ��      ,�  �  �      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   �      L�      ��  �  �      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  d�      ��      ؕ  �  �      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute ��      ��      ,�  �  �      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   �      T�      ��  �  �      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   h�      ��      �  �  �      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  Ȗ      �      8�  � 
       LOGICAL,INPUT pcMode CHARACTER  setUserProperty �      X�      ��  �        LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage h�      ȗ      ��  �         LOGICAL,INPUT pcMessage CHARACTER   Signature   ԗ      �      D�  � 	 ,      CHARACTER,INPUT pcName CHARACTER    <�    S	  ��   �      �      4   �����                �                      ��                  T	  �	                  dU                       T	  ��        U	  ,�  ��      �      4   �����                ��                      ��                  V	  �	                  �U                       V	  <�  ��    m	  ԙ  P�      �      4   �����                `�                      ��                  y	  {	                  lV                       y	  �         z	                                  �     
                    � ߱        �  $  }	  ��  ���                           $  	  �  ���                       �                         � ߱        H�    �	  X�  ԛ      �      4   �����                �                      ��                  �	  J
                  d]                       �	  h�  �  o   �	      ,                                 p�  $   �	  D�  ���                       P  @         <              � ߱        ��  �   �	  p      ��  �   �	  �      ��  �   �	  X      ��  �   �	  �      Ԝ  �   �	  @      �  �   �	  �      ��  �   �	  0      �  �   �	  l      $�  �   �	  �      8�  �   �	  T      L�  �   �	  �      `�  �   �	  L      t�  �   �	  �      ��  �   �	        ��  �   �	  �      ��  �   �	  �      ĝ  �   �	  0      ؝  �   �	  �      �  �   �	  �       �  �   �	  T      �  �   �	  �      (�  �   �	  D      <�  �   �	  �      P�  �   �	  4      d�  �   �	  �      x�  �   �	  $      ��  �   �	  �      ��  �   �	  �      ��  �   �	  H      Ȟ  �   �	  �      ܞ  �   �	  �      �  �   �	  4      �  �   �	  p      �  �   �	  �      ,�  �   �	  �      @�  �   �	  d      T�  �   �	  �      h�  �   �	  �      |�  �   �	        ��  �   �	  T      ��  �   �	  �      ��  �   �	  �      ̟  �   �	        ��  �   �	  D          �   �	  �                      �          x�  `�      ��                  q
  �
  ��              �_                    O   ����    e�          O   ����    R�          O   ����    ��      �     
                l                      |!                         � ߱        8�  $ �
  ��  ���                           O   �
  ��  ��  �!               ��          ��  ��    ��                                             ��                            ����                            �!  lK      ��      P�     @     ��                      V ��  �	                     �    �
  d�  �      �!      4   �����!                �                      ��                  �
  F                  �d                       �
  t�  �  �   �
  ("      �  �   �
  �"      ,�  �   �
  #      @�  �   �
  �#      T�  �   �
  $      h�  �   �
  �$      |�  �   �
   %      ��  �   �
  |%      ��  �   �
  �%      ��  �   �
  t&      ̣  �   �
  �&      �  �   �
  d'      ��  �   �
  �'          �   �
  \(      �    Q  $�  ��      �(      4   �����(                ��                      ��                  R  �                  �d                       R  4�  Ĥ  �   T  ,)      ؤ  �   U  �)      �  �   V  *       �  �   W  �*      �  �   X  +      (�  �   Y  x+      <�  �   Z  �+      P�  �   [  h,      d�  �   \  �,      x�  �   ]  P-      ��  �   ^  �-      ��  �   _  @.      ��  �   `  �.      ȥ  �   a  0/      ܥ  �   b  �/      �  �   c  (0      �  �   d  �0      �  �   e   1      ,�  �   f  �1      @�  �   g  2      T�  �   h  �2      h�  �   i  3      |�  �   j  �3      ��  �   k  4      ��  �   l  �4      ��  �   m   5      ̦  �   n  |5          �   o  �5      ��    �  ��  x�      `6      4   ����`6                ��                      ��                  �  �                  ��                       �  �  ��  �   �  �6      ��  �   �  <7      ħ  �   �  �7      ا  �   �  ,8      �  �   �  �8       �  �   �  9      �  �   �  �9      (�  �   �  �9      <�  �   �  8:      P�  �   �  t:      d�  �   �  �:      x�  �   �  $;      ��  �   �  �;      ��  �   �  <      ��  �     �<      Ȩ  �     �<      ܨ  �     p=      �  �     �=      �  �     h>      �  �     �>      ,�  �     ?      @�  �   	  �?      T�  �   
   @      h�  �     <@      |�  �     x@      ��  �     �@      ��  �     0A      ��  �     lA      ̩  �     �A      �  �     �A      ��  �      B      �  �     \B      �  �     �B      0�  �     C      D�  �     HC      X�  �     �C      l�  �     �C      ��  �     �C      ��  �     8D      ��  �     tD      ��  �     �D      Ъ  �     $E      �  �     �E      ��  �      F      �  �   !  �F       �  �   "  �F      4�  �   #  xG      H�  �   $  �G      \�  �   %  pH      p�  �   &  �H      ��  �   '  hI      ��  �   (  �I      ��  �   )   J      ��  �   *  \J      ԫ  �   +  �J      �  �   ,  �J          �   -  HK      T�  $  �  (�  ���                       �K     
                    � ߱        �    �  p�  ��      �K      4   �����K      /   �  ��     ��                          3   �����K            ܬ                      3   �����K  @�    �  �  ��  p�  L      4   ����L  	              ��                      ��             	     �  r                  �Ib                       �  �  ��  �   �  hL       �  $  �  ԭ  ���                       �L     
                    � ߱        �  �   �  �L      l�  $   �  @�  ���                       �L  @         �L              � ߱        (�  $  �  ��  ���                       0M                         � ߱        �M     
                 N                     pO  @        
 0O              � ߱        ��  V     Į  ���                        |O                     �O                     �O                         � ߱        H�  $    T�  ���                       �P     
                (Q                     xR  @        
 8R              � ߱        ذ  V   1  �  ���                        �R     
                 S                     PT  @        
 T              � ߱            V   V  t�  ���                        
              8�                      ��             
     t                                           t  �  \T     
                �T                     (V  @        
 �U          �V  @        
 LV          �V  @        
 �V          LW  @        
 W              � ߱            V   �  ��  ���                        adm-clone-props �  d�              �     A     `                          \  v                     start-super-proc    t�  в  �           �     B                                  �                     س    )  \�  l�      �Z      4   �����Z      /   *  ��     ��                          3   �����Z            ȳ                      3   ����[  0�  $  D  �  ���                       ([                         � ߱        �    T  L�  ȴ  h�  D[      4   ����D[                <�                      ��                  U  Y                  �@                       U  \�  X[                     l[                     �[                         � ߱            $  V  ش  ���                             Z  ��  ��      �[      4   �����[  �[                         � ߱            $  [  ��  ���                       �    b  �  �  p�  �[      4   �����[      $  c  D�  ���                       �[                         � ߱            �   �   \      @\     
                �\                     ^  @        
 �]              � ߱        �  V   �  ��  ���                        (�  �   �  ^      ��    I  D�  T�      X^      4   ����X^      /   J  ��     ��                          3   ����h^            ��                      3   �����^  |�  $  N  �  ���                       �^                         � ߱        �^     
                L_                     �`  @        
 \`              � ߱        ��  V   X  �  ���                        ��    �  ĸ  @�      �`      4   �����`                P�                      ��                  �  �                  �g                       �  Ը      g   �  h�         ��,�                           0�           �  �      ��                  �      �               h                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  \�     l�  �`                      3   �����`  ��     
   ��                      3   �����`         
   ��                      3   �����`    ��                              ��        �                  ����                                        |�              C      ̺                      g                               ��  g   �  ��          ��	4�                           h�          8�   �      ��                  �  �  P�              �h                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  ��     ��  a                      3   �����`            ļ                      3   ����a    ��                              ��        �                  ����                                        ��              D      Լ                      g                               ��  g   �  ��          ��	<�                           p�          @�  (�      ��                  �  �  X�              XVc                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  ��     ��  Ha                      3   ����,a            ̾                      3   ����Pa    ��                              ��        �                  ����                                        ��              E      ܾ                      g                               ��    �  ��  0�      la      4   ����la                @�                      ��                  �                    Wc                       �  Ŀ  ��  /   �  l�     |�                          3   ����|a            ��                      3   �����a  ��  /  �  ��     ��  �a                      3   �����a  �     
   �                      3   �����a  H�        8�                      3   �����a  x�        h�                      3   �����a            ��                      3   ���� b  ��      ��  ��      Db      4   ����Db      /     �     �  �b                      3   �����b  @�     
   0�                      3   �����b  p�        `�                      3   �����b  ��        ��                      3   �����b            ��                      3   ����c          ��  ��      4c      4   ����4c      /    (�     8�  �c                      3   ����hc  h�     
   X�                      3   �����c  ��        ��                      3   �����c  ��        ��                      3   �����c            ��                      3   �����c  ��      �  ��      �c      4   �����c                ��                      ��                                      ��                         $�      g     ��         ��\�        �c                  ��          P�  8�      ��                        h�              �                    O   ����    e�          O   ����    R�          O   ����    ��          /    ��     ��   d                      3   ����d  ��     
   ��                      3   ����,d         
   �                      3   ����4d    ��                            ����                                        ��              F      �                      g                               P�     "  <d                                     Pd     
                �d                     f  @        
 �e              � ߱        ��  V   �  ��  ���                        0f     
                �f                     �g  @        
 �g              � ߱        �  V   �  |�  ���                        ��    �  (�  8�      h      4   ����h      $   �  d�  ���                       ph  @         \h              � ߱        d�  g   �  ��         ���        �h  ���        �h                  ��          T�  <�      ��                  �    l�              �                    O   ����    e�          O   ����    R�          O   ����    ��              ��  ��      �h      4   �����h      O    ������  �h    ��                            ����                                        ��              G      ��                      g                               �  g   	  |�         �6��         �h                  D�          �  ��      ��                  
    ,�              �b                    O   ����    e�          O   ����    R�          O   ����    ��      \�      �h  }          O    ������  �h    ��                            ����                                        ��              H      t�                      g                               4�  g     (�         �"��                           T�          ��  ��      ����                 )  ��              |�b                    O   ����    e�          O   ����    R�          O   ����    ��                                    	       	                                  � ߱        ��  $     ��   �                       ��      ��  �      �h      4   �����h                (�                      ��                                       �b                         ��  l�  	    \�                                        3   ����i      O    ������  $i  ��      ��  �      8i      4   ����8i                ,�                      ��                    $                  �3d                         ��  ��  A          ��   ��         |�  �i                                         Xi   li                   ��  ��           xi  �i           �i  �i         �            ��   ��             �  ��      �i      4   �����i                ��                      ��                     #                  H�                           �  ��  	  !  ��                                        3   �����i      O  "  ������  �i  4�  /   &  $�                                 3   �����i      /   '  `�                                 3   ����j    ��                              ��        �                  ����                                              <�              I      p�                      g                               ��  g   1  L�         � 4�                            �          ��  ��      ��,�               2  C  ��              ��                    O   ����    e�          O   ����    R�          O   ����    ��      l�  $  5  @�  ���                       $j                         � ߱              7  ��  �  8�  8j      4   ����8j                �                      ��                  7  ?                  d�                       7  ��  ��  A  8        x�   ��         d�  �j                                         Xj   lj                   ��  ��           xj  �j           �j  �j         �            ��   ��    ��    9  ��  t�      �j      4   �����j                ��                      ��                  9  =                  ��                       9  �  ��  	  ;  ��                                        3   �����j      O  <  ������  �j      $   >  �  ���                       k  @         �j              � ߱            $   @  d�  ���                       (k  @         k              � ߱                      ��                                           ��                              ��        �                  ����                                  �          `�  ��         J     ��                      g   ��                          ��    ^  ��  (�      4k      4   ����4k                ��                      ��                  ^  �                  �`                       ^  ��  Dk  @                     pk  @         \k          �k  @         �k              � ߱        ��  $   _  8�  ���                       ��  g   e  ��         �nh�      }                      ��          x�  `�      ��                  f  j  ��              �`                    O   ����    e�          O   ����    R�          O   ����    ��      ��  /  g  ��                                 3   �����k        h   �  �      �k      4   �����k      O  i  ������  �k    ��                            ����                                        ��              K      (�                      g                               ��  g   o  ��         �!<�         l                  ��          t�  \�      ��                  o  q  ��              �u                    O   ����    e�          O   ����    R�          O   ����    ��      l  @                         � ߱            $  p  ��  ���                         ��                            ����                                        ��              L      ��                      g                               ��  /   t  ��                                 3   ����l        {  ��  l�      8l      4   ����8l                ��                      ��                  {  �                  tv                       {   �                (�          �  ��      ��                   �                  �v                         |�      O       ��          O       ��      d�  /   �  T�                                 3   ����Pl        �  ��  ��      pl      4   ����pl      k   �  ��              }       n        �   �  $   �  ��  ���                       �l  @         �l              � ߱            $   �  H�  ���                       �l  @         �l              � ߱        adm-create-objects  4�  t�                      M      �                               �!                     disable_UI  ��  ��                      N      <                              �!  
                   enable_UI   ��  L�                      O                                     �!  	                   exitObject  X�  ��                      P      �                               �!  
                   initializeObject    ��  �                      Q      �                              "                     procesa-parametros  0�  ��                      R      �                               "                     recoge-parametros   ��  ��                      S      �                               '"                     um-excel    �  l�          �         T     �                          �  k$                     um-procesar x�  ��          �          U                                 �%                     um-saldos-almacen   ��  <�  �       �      '   V     �                          �  �%                     �   ��   �  ���       �  %  �  < Todos los Clientes >� � ���  �          �  8   ����(   0�  8   ����(   @�  (  H�  8   ����&   X�  8   ����&   h�  & 
 p�  8   ����%   ��  8   ����%   ��  % 	 ��  8   ����$   ��  8   ����$   ��  $  ��  8   ����#   ��  8   ����#    �  #  ��  8   ����"   ��  8   ����"    �  8   ����!   �  8   ����!   (�  8   ����   8�  8   ����   H�    P�  8   ����   `�  8   ����   p�    x�  8   ����   ��  8   ����   ��        8   ����       8   ����             ��  ��      toggleData  ,INPUT plEnabled LOGICAL    ��  ��   �      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  ��  D�  P�      returnFocus ,INPUT hTarget HANDLE   4�  x�  ��      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    h�  ��  ��      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  (�  8�      removeAllLinks  ,   �  L�  \�      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE <�  ��  ��      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    ��  @�  L�      hideObject  ,   0�  `�  x�      editInstanceProperties  ,   P�  ��  ��      displayLinks    ,   |�  ��  ��      createControls  ,   ��  ��  ��      changeCursor    ,INPUT pcCursor CHARACTER   ��  �  �      applyEntry  ,INPUT pcField CHARACTER     �  H�  X�      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER 8�  ��  ��      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER ��  �  �      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE �  p�  ��      unbindServer    ,INPUT pcMode CHARACTER `�  ��  ��      startServerObject   ,   ��  ��  ��      runServerObject ,INPUT phAppService HANDLE  ��  �   �      restartServerObject ,   ��  4�  L�      initializeServerObject  ,   $�  `�  t�      disconnectObject    ,   P�  ��  ��      destroyServerObject ,   x�  ��  ��      bindServer  ,   ��  ��  ��      processAction   ,INPUT pcAction CHARACTER   ��  �  �      enableObject    ,   ��  0�  @�      disableObject   ,    �  T�  `�      applyLayout ,   D�  t�  ��      viewPage    ,INPUT piPageNum INTEGER    d�  ��  ��      viewObject  ,   ��  ��  ��      toolbar ,INPUT pcValue CHARACTER    ��   �  �      selectPage  ,INPUT piPageNum INTEGER    ��  8�  L�      removePageNTarget   ,INPUT phTarget HANDLE,INPUT piPage INTEGER (�  ��  ��      passThrough ,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER  x�  ��  ��      notifyPage  ,INPUT pcProc CHARACTER ��  �  �      initPages   ,INPUT pcPageList CHARACTER  �  H�  d�      initializeVisualContainer   ,   8�  x�  ��      hidePage    ,INPUT piPageNum INTEGER    h�  ��  ��      destroyObject   ,   ��  ��  ��      deletePage  ,INPUT piPageNum INTEGER    ��  �  �      createObjects   ,   ��  0�  @�      constructObject ,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE  �  ��  ��      confirmExit ,INPUT-OUTPUT plCancel LOGICAL  ��  ��  ��      changePage  ,   ��  �  $�      assignPageProperty  ,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER      � 
"     
 b%     adecomm/as-utils.w 
"   
   �    }        �
"     
   %              %       	       %              %       	           �     }        �G� 7   �G%              � ;  2   %         %       	 %        %       	%        %       	%               %               %               %              %              %              %               %              
�    � %              %              %              %         %          �       
�             �G%              %               %     _corre-program  %      ENTRY   
"  
 
   %      ENTRY   
"  
 
   
"   
 ��       (    �A�    �
"   
 ��        T     %               
"   
 ��        �     %               (    S    �     }         � #    %               %                   �     }         � 0    %     bin/_inslook.r  �     }        �"      � 8         �     }         � 0    
"   
 b    �        �     %              � >     
"   
   (    S    �     }         � #    %               %                   �     }         � 0    
�     }        �G
�     }        � %     _busca-lookup   �     }        �"      "          "    b�     �
"   
 ��             %               
"   
 ��        D     %               
"  
 
 �        x    6@� E     � M     � U   �� g     � l   b%               
"   
 b    �        �     � }    
"   
 ��             %              
"   
 ��        P     �     }         
"   
 ��        �          �     }         �     }        �
"   
 ��        �    ��     }        �
"   
 ��             %               
"   
   �        <     %              , (   (     
�     }        �
"   
 �    �     }        �G� �   �G
"   
 ��        �     %               
"   
 ��         	     %               %      notify  � �     %      notify  � �     "    �"    �&    &    &    &        %              %              *    "      "      � �   �"    �&    &    &    &        %              %              *    "      "      �     b�       �    }        �� �     "      � U     %     bin/_calc.r     �  %              
"  
 
   �            B�  � l     %     bin/_calenda.r      �  %              
"  
 
   �        x    B�  �      %     recoge-parametros �"      "          "    b%              
"  
 
   �            B"      %     procesa-parametros �    }        ��           
"   
 b
�    
"   
 b
"   
     �        �     �        �    
"   
   �        �         �     }        �%              
"   
 b
"   
     �        (     �        4    
"   
   �        p         �     }        �%              � 
"    
 �%              � �  �         X      $              
�    � 6   �     
"   
                       
�            � 8   
"    
 
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        0    7%               
"   
 ��           d    1� H  
 �� S   �%               o%   o           � X    �
"   
 ��           �    1� Y   �� S   �%               o%   o           � g   �
"   
 ��           L    1� n  
 �� S   �%               o%   o           � y   �
"   
 ��           �    1� �   �� S   �%               o%   o           � �   �
"   
 ��           4    1� �   �� S   �%               o%   o           � �   �
"   
 ��           �    1� �   �� �   �%               o%   o           %               
"   
 ��          $    1� �   �� �     
"   
 ��           `    1� �   �� S   �%               o%   o           � �  e �
"   
 ��           �    1� d   �� S   �%               o%   o           � s  [ �
"   
 ��           H    1� �   �� �   �%               o%   o           %               
"   
 ��           �    1� �   �� �   �%               o%   o           %               
"   
 ��           @    1� �   �� �   �%               o%   o           %              
"   
 ��          �    1� �   �� �     
"   
 ��           �    1�   
 �� �   �%               o%   o           %               
"   
 ��           t    1�    �� S   �%               o%   o           � X    �
"   
 ��          �    1�     �� �     
"   
 ��           $    1� 0   �� S   �%               o%   o           � F  t �
"   
 ��          �    1� �  
 �� �     
"   
 ��           �    1� �   �� S   �%               o%   o           � �  � �
"   
 ��           H    1� d   �� S   �%               o%   o           � X    �
"   
 ��           �    1� {  
 �� �   �%               o%   o           %               
"   
 �           8    1� �   � �   �%               o%   o           %               
"   
 b�           �    1� �   b� S   �%               o%   o           � X    
"   
 b�           (    1� �   b� S   �%               o%   o           o%   o           
"   
 �           �    1� �  
 � S   �%               o%   o           � X    
"   
 b�               1� �   b� �  	 �%               o%   o           � �  / 
"   
 ��          �    1� 	   �� �  	   
"   
 �           �    1�    � �  	 �o%   o           o%   o           � X    
"   
 ��          <    1� .   �� �  	   
"   
 �           x    1� =   � �  	 �o%   o           o%   o           � X    
"   
 ��          �    1� M   �� �     
"   
 ��          (    1� [   �� �  	   
"   
 ��          d    1� h   �� �  	   
"   
 ��          �    1� u   �� �  	   
"   
 �           �    1� �   � �   �o%   o           o%   o           %              
"   
 ��          X    1� �   �� �  	   
"   
 ��          �    1� �  
 �� �     
"   
 ��          �    1� �   �� �  	   
"   
 ��              1� �   �� �  	   
"   
 ��          H    1� �   �� �  	   
"   
 ��          �    1� �   �� �  	   
"   
 ��          �    1� �  	 �� �  	   
"   
 ��          �    1�    �� �  	   
"   
 ��          8    1�    �� �  	   
"   
 b�           t    1� /   b� S   �%               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 
"   
   
"   
 (�  L ( l       �        <     �� ;   � P   �        H     �@    
� @  , 
�       T     �� D     p�               �L
�    %              � 8      `     � $         � K          
�    � e     
"   
 �� @  , 
�       p!    �� n  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 �           "    1� h  
 � S   �%               o%   o           � X    
"   
 �           �"    1� s  
 � S   �%               o%   o           o%   o           
"   
 �           #    1� ~   � �   �%               o%   o           o%   o           
"   
 b�           �#    1� �   b� �   �%               o%   o           %               
"   
 �           $    1� �   � �   �%               o%   o           %               
"   
 b�           �$    1� �   b� S   �%               o%   o           � X    
"   
 �           �$    1� �   � �   �%               o%   o           %              
"   
 �           p%    1� �   � �   �%               o%   o           o%   o           
"   
 �           �%    1� �   � S   �%               o%   o           o%   o           
"   
 �           h&    1� �  	 � S   �%               o%   o           � X    
"   
 �           �&    1� �   � S   �%               o%   o           o%   o           
"   
 �           X'    1� �   � S   �%               o%   o           o%   o           
"   
 �           �'    1�    � �   �%               o%   o           %               
"   
 �           P(    1�    � �   �%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 �            )    1�    � �  	 �%               o%   o           � X    
"   
 �           �)    1� ,   � �  	 �%               o%   o           � X    
"   
 �           *    1� :   � �   �%               o%   o           %               
"   
 b�           �*    1� H   b� �  	 �%               o%   o           � X    
"   
 �           �*    1� W   � �  	 �%               o%   o           � X    b
"   
 �           l+    1� e   � �   �%               o%   o           %               
"   
 b�           �+    1� s   b� �  	 �%               o%   o           � X    
"   
 �           \,    1� �   � �  	 �%               o%   o           � X    b
"   
 �           �,    1� �   � �  	 �%               o%   o           � X    
"   
 �           D-    1� �   � �  	 �%               o%   o           o%   o           
"   
 �           �-    1� �   � �  	 �%               o%   o           � X    
"   
 b�           4.    1� �   b� �  	 �%               o%   o           � X    
"   
 �           �.    1� �  	 � �   �%               o%   o           %               
"   
 �           $/    1� �   � �   �%               o%   o           %               
"   
 �           �/    1� �   � �   �%               o%   o           o%   o           
"   
 b�           0    1� �   b� �   �%               o%   o           o%   o           
"   
 �           �0    1� �   � �   �%               o%   o           %               
"   
 �           1    1�    � �   �%               o%   o           %               
"   
 �           �1    1�    � �   �%               o%   o           %               
"   
 b�           2    1� 2   b� >   �%               o%   o           %       
       
"   
 b�           �2    1� F   b� >   �%               o%   o           o%   o           
"   
 �           3    1� R   � >   �%               o%   o           %              
"   
 �           �3    1� ^   � >   �%               o%   o           o%   o           
"   
 �           �3    1� j   � >   �%               o%   o           %              
"   
 �           x4    1� w   � >   �%               o%   o           o%   o           
"   
 �           �4    1� �   � >   �%               o%   o           %              
"   
 �           p5    1� �   � >   �%               o%   o           o%   o           
"   
 b�           �5    1� �   b� �  	 �%               o%   o           � X    P �L 
�H T   %              �     }        �GG %              
"   
 �           �6    1� �   � �   �%               o%   o           %               
"   
 �           07    1� �   � �   �%               o%   o           o%   o           
"   
 �           �7    1� �   � S   �%               o%   o           � X    
"   
 �            8    1� �   � S   �%               o%   o           � �  - 
"   
 �           �8    1�    � S   �%               o%   o           � X    
"   
 �           9    1� )   � S   �%               o%   o           � F   
"   
 ��          |9    1� d   �� �     
"   
 �           �9    1� u   � S   �%               o%   o           � X    
"   
 ��          ,:    1� �  
 �� �     
"   
 ��          h:    1� �   �� �     
"   
 �           �:    1� �   � �  	 �%               o%   o           � X    
"   
 �           ;    1� �   � S   �%               o%   o           � X    
"   
 �           �;    1� �   � �   �%               o%   o           o%   o           
"   
 �           <    1� �   � S   �%               o%   o           � �  ! b
"   
 �           |<    1� �   � S   �%               o%   o           � X    
"   
 b�           �<    1�    b� S   �%               o%   o           �    
"   
 b�           d=    1� $  	 b� �   �%               o%   o           o%   o           
"   
 �           �=    1� .   � �   �%               o%   o           %               
"   
 ��          \>    1� :   �� �     
"   
 �           �>    1� H   � S   �%               o%   o           � \   
"   
 b�           ?    1� k   b� �  	 �%               o%   o           � X    
"   
 �           �?    1� x   � �  	 �%               o%   o           � X    b
"   
 ��          �?    1� �   �� �     
"   
 ��          0@    1� �   �� �  	   
"   
 b�           l@    1� �   b� �   �o%   o           o%   o           %               
"   
 ��          �@    1� �   �� �     
"   
 ��          $A    1� �   �� �  	   
"   
 ��          `A    1� �   �� �  	   
"   
 ��          �A    1� �   �� �  	   
"   
 ��          �A    1�    �� �  	   
"   
 ��          B    1�    �� �  	   
"   
 ��          PB    1� /   �� �     
"   
 �           �B    1� @   � S   �%               o%   o           � W  4 
"   
 ��           C    1� �   �� �     
"   
 ��          <C    1� �   �� �     
"   
 ��          xC    1� �   �� �     
"   
 ��          �C    1� �   �� �  	   
"   
 ��          �C    1� �   �� �  	   
"   
 ��          ,D    1� �   �� �  	   
"   
 ��          hD    1� �   �� �     
"   
 �           �D    1� �   � �  	 �%               o%   o           � X    
"   
 �           E    1� 	   � �  	 �%               o%   o           � X    
"   
 �           �E    1�    � �  	 �%               o%   o           � X    
"   
 �            F    1� *   � �  	 �%               o%   o           � X    
"   
 �           tF    1� ?   � �   �%               o%   o           %               
"   
 �           �F    1� M   � �   �%               o%   o           o%   o           
"   
 b�           lG    1� _   b� �   �%               o%   o           %               
"   
 �           �G    1� o   � �   �%               o%   o           %               
"   
 �           dH    1� {   � �   �%               o%   o           o%   o           
"   
 �           �H    1� �   � �   �%               o%   o           %               
"   
 ��          \I    1� �   �� �  	   
"   
 �           �I    1� �   � �   �%               o%   o           %              
"   
 ��          J    1� �   �� �  	   
"   
 ��          PJ    1� �   �� �  	   
"   
 ��          �J    1� �  
 �� �  	   
"   
 �           �J    1� �   � �  	 �%               o%   o           � ?   
"   
 �           <K    1� �   � �  	 �%               o%   o           � X    
"   
    "    �%     start-super-proc ��%     adm2/smart.p �P �L 
�H T   %              �     }        �GG %              
"   
   �       \L    6� ;     
"   
   
�        �L    8
"   
   �        �L    ��     }        �G 4              
"   
 ߱G %              G %              %p e `   LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout 
�H T   %              �     }        �GG %              
"   
 
"   
 �
"   
 
"   
   (�  L ( l       �        �M    �� ;   � P   �        �M    �@    
� @  , 
�       N    �� D   p�               �L
�    %              � 8      N    � $         � K          
�    � e   
"   
 �p� @  , 
�       $O    �� �   �p�               �L"    , �   � 8   � :   ��     }        �A      |    "      � 8   %              (<   \ (    |    �     }        �A� <   �A"        "    "      < "    "    (    |    �     }        �A� <   �A"    
�H T   %              �     }        �GG %              
"   
 
"   
 �
"   
 
"   
   (�  L ( l       �        �P    �� ;   � P   �        Q    �@    
� @  , 
�       Q    �� D   p�               �L
�    %              � 8      Q    � $         � K          
�    � e   
"   
 �p� @  , 
�       ,R    �� H  
 �p�               �L"    , 
�H T   %              �     }        �GG %              
"   
 
"   
 �
"   
 
"   
   (�  L ( l       �        �R    �� ;   � P   �        �R    �@    
� @  , 
�       �R    �� D   p�               �L
�    %              � 8      �R    � $         � K          
�    � e   
"   
 �p� @  , 
�       T    �� �   �p�               �L
"   
 , 
�H T   %              �     }        �GG %              
"   
   
"   
 
"   
   
"   
   (�  L ( l       �        �T    �� ;   � P   �        �T    �@    
� @  , 
�       �T    �� D     p�               �L
�    %              � 8      �T    � $         � K          
�    � e     
"   
 �p� @  , 
�       �U    �� n  
 �p�               �L%     SmartWindow 
"   
   p� @  , 
�       @V    �� �     p�               �L%      WINDOW  
"   
  p� @  , 
�       �V    �� =    p�               �L%               
"   
  p� @  , 
�        W    ��     p�               �L(        � X      � X      � X      �     }        �A
�H T   %              �     }        �GG %              
"   
  (   � 
"   
     �        �W    �� ;   �
"   
   � 8      ,X    � $         � K          
�    � e   
"   
   �        �X    �
"   
   �       �X    /
"   
   
"   
   �       �X    6� ;     
"   
   
�        �X    8
"   
   �        Y    �
"   
   �       <Y    �
"   
   p�    � e   
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
     �         Z    �A"    �A
"   
   
�        LZ    �@ � 
"   
 "      �       }        �
"   
 �%              %                "    �%     start-super-proc ��%     adm2/appserver.p �    � �     
�    �     }        �%               %      Server  - �     }        �    "    � X    �%                   "    � X    �%      NONE    p�,  8         $     "    b        �      
�    
�H T   %              �     }        �GG %              
"   
 
"   
 �
"   
 
"   
   (�  L ( l       �        �\    �� ;   � P   �        �\    �@    
� @  , 
�       �\    �� D   p�               �L
�    %              � 8      �\    � $         � K          
�    � e   
"   
 �p� @  , 
�       �]    �� �   �p�               �L"    , p�,  8         $     "    b        �     
�     "    �%     start-super-proc ��%     adm2/visual.p �   � 6     � 2      � 4   (   
�H T   %              �     }        �GG %              
"   
 
"   
 �
"   
 
"   
   (�  L ( l       �        _    �� ;   � P   �        (_    �@    
� @  , 
�       4_    �� D   p�               �L
�    %              � 8      @_    � $         � K          
�    � e   
"   
 �p� @  , 
�       P`    �� s   �p�               �L"    , � 
" 	   
 �%     contextHelp 
" 	   
   
�    
�    %     processAction   
�    %     CTRL-PAGE-UP �%     processAction   
�    %     CTRL-PAGE-DOWN  "    �%     start-super-proc �%     adm2/containr.p %     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents %      initializeDataObjects 0 0   A    �    � �    
�    � �    �A    �    � �      
�    � �    �%     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents %     buildDataRequest ent0 A    �    � �    �
�    � �    %     modifyListProperty 
�    
�    %      Add     %     SupportedLinks %      ContainerToolbar-Target � 
" 	   
 �
"   
 �%     contextHelp 
" 	   
   
�    
�    %               
�H T   %              �     }        �GG %              
"   
 
"   
 �
"   
 
"   
 (�  L ( l       �        �d    �� ;   � P   �        �d    �@    
� @  , 
�       �d    �� D   p�               �L
�    %              � 8      �d    � $         � K        
�    � e   �
"   
 �p� @  , 
�       �e    �� �   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
 
"   
 �
"   
 
"   
 (�  L ( l       �        |f    �� ;   � P   �        �f    �@    
� @  , 
�       �f    �� D   p�               �L
�    %              � 8      �f    � $         � K        
�    � e   
"   
 �p� @  , 
�       �g    �� ?   �p�               �L%              (        �     }        �G� 7   �G� 
"   
 
"   
   �        Ph    �%              
"   
 �
"   
 ��     }        �%               
"   
 �%      CLOSE   %                   "    "  	  �� !     %                   "    �     �%               "      &    &    &    &        %              %               *    � L!     %               %     um-procesar %     um-excel ��     }        B    "    �     �%               "      &    &    &    &        %              %               *    � L!     %               �            B"      �            B� �     � 
"   
 �
"   
 
"   
 �        Pk    %%              
�     }        �
"   
   %     destroyObject       �     }        �    �  � u!  	   %               
"   
 �
�    %     createObjects    �     }        �%     initializeObject � �     }        ��            B         +  %              � !  
 �            B    +  � !  
   (        �     }        �G� 7   �G� 
"   
 
"   
   �     }        �
�    
"   
 "    "    "      "  	    "      "      "  
    
"   
 
"   
   %      CLOSE   %               %      SUPER   �            B         +  %              � !  
 �            B    +  � !  
   �            B"          "    �     �"     �"    �&    &    &    &        %              %              *    �            B"      �            B� �!     �            B� �!     
"  
 
 �        �o    ��       
"  
 
 �        �o    ��       �    }        �� �     �            B� �"     � �"     "      lp                              � �"     p�  @p  Lp         %               "      �p                              �p                             � �"  	   � �      �  �p  �p        "      dq                              �q        pq  tq  xq          � �"     |q         %              � �"     �  q  $q        "      4r        r   r  $r          @r                              Lr                              (r         � �"     � �"     � �"     � �"     p�  �q  �q         %              "      s        �r  �r  �r          s                              s                              �r         � �"     � �"     � �"     � �"     p�  �r  �r         %              "      �s        �s  �s  �s          �s                              �s                              �s         � #     � �"     � �"     � �"     p�  Ps  \s         %              "      �t        lt  pt  tt          �t                              xt         � 	#     � �"     � #     p�   t  ,t          <     (         � #   ߱�             B� #   B�            B"      lu        Tu  Xu  \u          xu                              `u         �  #     � �"     � #     p�  u  u          <     (         � ##   ߱�            B� #   B�            B"      Tv        <v  @v  Dv          `v                              Hv         � /#     � �"     � #     p�  �u  �u          <     (         � 2#   ߱�            B� F#   B�            B"      \w        Dw  Hw  Lw          hw                              tw                              Pw         � T#     � �"     � �"     � �"     p�  �v  �v         %              "      x        �w  �w  �w          x                               x         � Z#     � �"     � #     p�  �w  �w         � ]#     "      �x        �x  �x  �x          �x                              �x         � d#     � �"     � #     p�  Dx  Px         � g#     "      Dy        ,y  0y  4y          Py                              8y         � {#     � �"     � #     p�  �x  �x         � ~#     "      �y        �y  �y  �y          �y                              �y         � �#     � �"     � #     p�  |y  �y         � �#  
   "      |z        dz  hz  lz          �z                              pz         � �#     � �"     � #     p�  z  $z         � �#     "      {         {  {  {          ${                              {         � �#     � �"     � #     p�  �z  �z         � �#     "      �{        �{  �{  �{          �{                              �{         � �#     � �"     � #     p�  P{  \{         � �#     "      P|        8|  <|  @|          \|                              D|         � �#     � �"     � #     p�  �{  �{         � �#     "      �|        �|  �|  �|          �|                              �|         � �#     � �"     � #     p�  �|  �|         � �#     "      �}        p}  t}  x}          �}                              |}         � �#     � �"     � #     p�  $}  0}         � �#     "      $~        ~  ~  ~          0~                              ~         � �#     � �"     � #     p�  �}  �}         � �#     "      �~        �~  �~  �~          �~                              �~         � �#     � �"     � #     p�  \~  h~         � �#     "      \        D  H  L          h                              P         � �#     � �"     � #     p�  �~           � $     "      �        �  �  �          �                              �         � $     � �"     � #     p�  �  �         � $     "      ��        |�  ��  ��          ��                              ��         � $     � �"     � #     p�  0�  <�         � $     "      0�        �  �   �          <�                              $�         � $     � �"     � #     p�  ̀  ؀         � $     "      ́        ��  ��  ��          ؁                              ��         � $     � �"     � #     p�  h�  t�         � $  	   %                   "      %                   "           � '$     "  	    "      ؂        ��  Ă  Ȃ          �                              ̂         "  
    � �"     � #     p�  t�  ��              � )$     "           � +$     "  	    "      ��        ��  ��  ��          ��                              ��         "  
    � �"     � #     p�  D�  P�              � )$     "           � -$     "  	    "      x�        `�  d�  h�          ��                              l�         "  
    � �"     � #     p�  �   �              � )$     "           � /$     "  	    "      H�        0�  4�  8�          T�                              <�         "  
    � �"     � #     p�  �  ��              � )$     "           � 1$     "  	    "      �         �  �  �          $�                              �         "  
    � �"     � #     p�  ��  ��              � )$     "           � 3$     "  	    "      �        І  Ԇ  ؆          �                              ܆         "  
    � �"     � #     p�  ��  ��              � )$     "  
         � 5$     "  	    "      ��        ��  ��  ��          ć                              ��         "  
    � �"     � #     p�  T�  `�         "           � 7$     "  	    "      t�        \�  `�  d�          ��                              h�         "  
    � �"     � #     p�  �  �         "           � 9$     "  	    "      0�        �  �   �          <�                              $�         "  
    � �"     � #     p�  ̈  ؈         "           � ;$     "  	    "      �        ԉ  ؉  ܉          ��                              ��         "  
    � �"     � #     p�  ��  ��         "           � =$     "  	    "      ��        ��  ��  ��          ��                              ��         "  
    � �"     � #     p�  D�  P�         "           � ?$     "  	    "      d�        L�  P�  T�          p�                              X�         "  
    � �"     � #     p�   �  �              "      "           � A$     "  	    "      4�        �   �  $�          @�                              (�         "  
    � �"     � #     p�  Ћ  ܋         "           � C$     "  	    "      ��        ،  ܌  ��          ��                              �         "  
    � �"     � #     p�  ��  ��         "           � E$     "  	    "      ��        ��  ��  ��          ��                              ��         "  
    � �"     � #     p�  H�  T�         "           � G$     "  	    "      h�        P�  T�  X�          t�                              \�         "  
    � �"     � #     p�  �  �         "           � I$     "  	    "      $�        �  �  �          0�                              �         "  
    � �"     � #     p�  ��  ̎          4               "    ߱"      "      "      "      ď                              � �"     p�  ��  ��         %              "      $�                              � K$     p�  ��  �         %               "      "      "      "      �    }        ��       � Y$     �            B� $     �    }        �� �     � �$   �� 1$   �� G$   �"     �"    �"  	  �&    &    &    &    x @   T    0        %              %                  " !     &        " !     &            " !     &        " !     &              "      �           "      " !               "      �           " !     "      " !   �" !   �" !   �" !   �&    &    &    &    &    &    p     T    8        %                  " " !    &    %              %                  " "     " "         " "     " "     " "   �&    &     *    " "     "     �" "   �&    &    &    &        %              %              * #   "     �" #   �&    &    &    &        %              %              "     �" #   �" # $  �&    &    &    &    &    &    0        %              %              %              � P%   �" #   �&    &    &    &        %              %              " #       <   " #     (    * $        � #     " $     �         <   " # $   (    * %        � #     " %     �         <   " #    (    * &        � #     " &     �       " #         "      "            "      " "          "      " "     �            B� S%  !   %               %     um-saldos-almacen �� u%   a"    a+  "     a     "      "       %               %     um-saldos-almacen �� y%   a"    a+  "     a     "      "       %               %     um-saldos-almacen �� }%   a"    a+  "     a     "      "       %               %     um-saldos-almacen �� �%   a"    a+  "     a     "      "       %               %     um-saldos-almacen �� �%   a"    a+  "     a     "      "       %               %     um-saldos-almacen �� �%   a"    a+  "     a     "      "       �    }        ��       %               "     �" '   �" '   �" '   �&    &    &    &    &    &    &    L    0        %              %              %              %              * (   " (                     �           �   l       ��                  D  F  �               ��c                    O   ����    e�          O   ����    R�          O   ����    ��          /   E  �      �                           3   ����4	                                  3   ����H	    ��                            ����                                            �           �   l       ��                  P  R  �               d�c                    O   ����    e�          O   ����    R�          O   ����    ��          /   Q  �      �                           3   ����T	                                  3   ����h	    ��                            ����                                            ,          �   l       ���               \  w  �               �a                    O   ����    e�          O   ����    R�          O   ����    ��      �       �              �          �                    �          �                               �  A  a        �   ��         |  �	                                        t	   �	                   �  �           �	  �	           �	  �	         �            �   �          d    �  |  �	      4   �����	                �                      ��                  d  h                  �Jc                       d     �	                     �	                         � ߱            $  e  �  ���                                     �                      ��                  i  u                  4Kc                       i    T  A  j        �   ��         �  4
                                        �	   
                   @  4           
  $
           
  ,
         �                          m  p  �  <  d
      4   ����d
  l
                     x
                         � ߱            $  n  �  ���                       �
                     �
                         � ߱            $  r  �  ���                                     �                                           ��                            ����                                                  �           �   l       ��                 �  �  �               ��a                    O   ����    e�          O   ����    R�          O   ����    ��      x  $  �  �   ���                       �
                         � ߱                      �          �  �      ��                 �  �  �              �"c                x     �        O   �    ��          O   �    ��          O   �    ��          p   �  �
  �     �  8  h     �
                x                      ��                  �  �                  |tc                       �  �  �  /   �  �                                 3   �����
        �  �  �      �
      4   �����
      $   �    ���                       ,  @                       � ߱        �  �     0                �                      ��                  �  �                  �tc                       �  H     /   �  �                                 3   ����<        �    ,      X      4   ����X      $   �  X  ���                       �  @         �              � ߱                   �                                      ��                  �  �                  |uc                       �  �  L  /   �  <                                 3   �����  �  /   �  x     �                          3   �����            �                      3   �����  <    �  �  �      �      4   �����      $   �    ���                       (  @                       � ߱            /   �  h                                 3   ����4      $  �  �  ���                       T                         � ߱                     $                                                             ��                            ����                                            �           �   l       ��                 �  �  �               ��                    O   ����    e�          O   ����    R�          O   ����    ��        $  �  �   ���                       �W     
                    � ߱              �  (  �      �W      4   �����W                �                      ��                  �  �                  t�                       �  8  �  �  �  8X            �  �  `      �X      4   �����X                p                      ��                  �  �                  ��                       �  �  �  o   �      ,                                 �  �   �  �X      �  �   �  �X      $  $  �  �  ���                       Y     
                    � ߱        8  �   �  (Y      L  �   �  HY      `  �   �  hY          $   �  �  ���                       �Y  @         �Y              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �  
  �               (�                    O   ����    e�          O   ����    R�          O   ����    ��      �                      �          �  $  �    ���                       �Y     
                    � ߱                  �  �                      ��                   �  �                  ��                     �  4      4   ����Z      $  �  �  ���                       XZ     
                    � ߱        �    �  4  D      lZ      4   ����lZ      /  �  p                               3   �����Z  �  �   �  �Z          O     ��  ��  �Z                               , �                          
                               �      ��                            ����                                                        �   l       ��                  �  �  �               ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                                            �           �   l       ��                  �  �  �               T�                    O   ����    e�          O   ����    R�          O   ����    ��           �  �   �       �l      4   �����l      n   �     �          <m        �    ,      Hm      4   ����Hm      �   �  \m    ��                            ����                                            4          �   l       ��                  �  �  �               |�                    O   ����    e�          O   ����    R�          O   ����    ��      pm  �           |m  �          �m  �          �m  �          �m  �          �m  �          �m  �              � ߱        �  Z   �  �    �        dm                  �               �              �              �              � ߱        �  h   �  `   �        �m                  
   �  �� �             �m    ��                              ��        �                  ����                                            �           �   l       ��                  �  �  �               d�                    O   ����    e�          O   ����    R�          O   ����    ��      �     �  �m  }          O   �  ��  ��  �m    ��                            ����                                            �           �   l       ���               �  �  �               �)                    O   ����    e�          O   ����    R�          O   ����    ��      \  /   �  �                                 3   ����n                l                      ��                  �  �                  (*                       �  �   �  $   �  �  ���                       ,n  @         n              � ߱          $   �  �  ���                       tn  @         `n              � ߱        t  $   �  H  ���                       �n  @         �n              � ߱              �  �    <  �n      4   �����n                                      ��                  �  �                  �                       �  �  �  A  �        �   ��         l  o                                         �n   �n                   �  �           �n  �n           �n  �n         �            �   �          �     |  �  4o      4   ����4o                �                      ��                  �  �                  `K                       �        $   �  �  ���                       Po  @         <o              � ߱            $   �    ���                       po  @         \o              � ߱            $   �  h  ���                       �o  @         |o              � ߱          ��                              ��        �                  ����                                                  �           �   l       ��                      �               �                    O   ����    e�          O   ����    R�          O   ����    ��          p     �o  �                    �o    ��                            ����                                            �           �   l       ��                    2  �               �                    O   ����    e�          O   ����    R�          O   ����    ��          p   '  �o  �       /             �o    ��                            ����                                            �           �   l       ��                 8  �  �               ��                    O   ����    e�          O   ����    R�          O   ����    ��      4  �   L  �o                    D                      ��                  M  O                  �v                �     M  �       $   N  p  ���                       (p  @         p              � ߱        �  o   R           4p                          (  $   U  �  ���                       �p  @        	 xp              � ߱        �  $  X  T  ���                       q                         � ߱        �  $  [  �  ���                       �q                         � ߱        0  $   _    ���                       lr  @        	 Xr              � ߱        �  $   `  \  ���                       <s  @        	 (s              � ߱        L  $   a  �  ���                       t  @        	 �s              � ߱                      \                      ��                  g  n                  �                d     g  �  �  $   h  �  ���                       �t  @        	 �t              � ߱          $   j  �  ���                       �u  @        	 �u              � ߱            $   l  8  ���                       �v  @        	 lv              � ߱        �  $   o  �  ���                       �w  @        	 �w              � ߱          $   p  �  ���                       8x  @        	 $x              � ߱        l  $   q  @  ���                       �x  @        	 �x              � ߱        �  $   r  �  ���                       py  @        	 \y              � ߱          $   s  �  ���                       z  @        	 �y              � ߱        t  $   t  H  ���                       �z  @        	 �z              � ߱        �  $   u  �  ���                       D{  @        	 0{              � ߱        $  $   w  �  ���                       �{  @        	 �{              � ߱        |  $   x  P  ���                       ||  @        	 h|              � ߱        �  $   y  �  ���                       }  @        	 }              � ߱        ,	  $   {   	  ���                       �}  @        	 �}              � ߱        �	  $   |  X	  ���                       P~  @        	 <~              � ߱        �	  $   }  �	  ���                       �~  @        	 �~              � ߱        4
  $   ~  
  ���                       �  @        	 t              � ߱        �
  $     `
  ���                       $�  @        	 �              � ߱        �
  $   �  �
  ���                       ��  @        	 ��              � ߱        <  $   �    ���                       \�  @        	 H�              � ߱        �  $   �  h  ���                       ��  @        	 �              � ߱        X  $  �  �  ���                       �                         � ߱              h      �          �  �      ��                  �  �  �              �b                \     �  �      �  �       ��                            7   ����         ��                     �            4                  6   �       X   ��                    �            4                                                                �  �                                   @            t   �        O   ����  e�          O   ����  R�          O   ����  ��      T  $  �  (  ���                       �                         � ߱        �  $  �  �  ���                       @�       	       	           � ߱          $  �  �  ���                       T�       
       
           � ߱        \  $   �  0  ���                       �  @        	 ��              � ߱        �  $  �  �  ���                       $�       
       
           � ߱          $   �  �  ���                       ԃ  @        	 ��              � ߱        d  $  �  8  ���                       �       
       
           � ߱        �  $   �  �  ���                       ��  @        	 ��              � ߱          $  �  �  ���                       Ą       
       
           � ߱        l  $   �  @  ���                       t�  @        	 `�              � ߱        �  $  �  �  ���                       ��       
       
           � ߱          $   �  �  ���                       D�  @        	 0�              � ߱        t  $  �  H  ���                       d�       
       
           � ߱        �  $   �  �  ���                       �  @        	  �              � ߱        $  $  �  �  ���                       4�       
       
           � ߱        |  $   �  P  ���                       �  @        	 Ї              � ߱        �  $  �  �  ���                       ��       
       
           � ߱        ,  $   �     ���                       ��  @        	 ��              � ߱        �  $  �  X  ���                       ��       
       
           � ߱        �  $   �  �  ���                       \�  @        	 H�              � ߱        4  $  �    ���                       h�       
       
           � ߱        �  $   �  `  ���                       �  @        	 �              � ߱        �  $  �  �  ���                       $�       
       
           � ߱        <  $   �    ���                       Ԋ  @        	 ��              � ߱        �  $  �  h  ���                       ��       
       
           � ߱        �  $   �  �  ���                       ��  @        	 |�              � ߱        D  $  �    ���                       ��       
       
           � ߱        �  $   �  p  ���                       `�  @        	 L�              � ߱        �  $  �  �  ���                       l�       
       
           � ߱        L  $   �     ���                       �  @        	 �              � ߱        �  $  �  x  ���                       (�       
       
           � ߱        �  $   �  �  ���                       ؍  @        	 č              � ߱        T  $  �  (  ���                       �       
       
           � ߱        �  $   �  �  ���                       ��  @        	 ��              � ߱          $  �  �  ���                       ��       
       
           � ߱            $   �  0  ���                       P�  @        	 <�              � ߱        �  $   �  �  ���                       �  @        	 Џ              � ߱          $   �  �  ���                       D�  @        	 0�              � ߱          �  �  X�  ,  �  �  d�  <  �  �  p�  L  �  �  |�  `  �   �  ��          	  �  �                                        3   ������                �                                   ����            ��                              ��        �                   ��                            ����                                            �           �   l       ���          	     �  /  �               �                    O   ����    e�          O   ����    R�          O   ����    ��      0  �   �                   @                      ��                  �  �                  �Z                �     �  �       $   �  l  ���                       Ȑ  @         ��              � ߱          �   �  Ԑ            (      �          �  �      ��                  �    �              )                8     �  �      T  �       ��                            7   ����    !      ��               \�    �            �                  6   �       ! D   ��           \�    �            �                                                        ��    �   �   �   $�   0�                   �  �           <�  L�           D�  T�                      `   t        O   ����  e�          O   ����  R�          O   ����  ��            �    �      �      4   �����                �                      ��                  �                    ��                       �          �  �  0      d�      4   ����d�                �                      ��                  �                    �                       �  �        �      �  �      P  8      ��                  �    h              p�                       �  @      �  8       ��                            7   ����    "      ��               �    �            �                  6   �       " �   ��         �  �    �            �                                                        ��   ��   Ē   В                   $             ܒ  �  ��           �  ��  �                      �            O   ����  e�          O   ����  R�          O   ����  ��      �  $  �  �  ���                       ��                          � ߱        �  A  �       4   ��        	 (                                             ē                 |  p           Г           ؓ         �            P   `    �    �  �  (	      ��      4   ������                8	                      ��                  �  �                  ��                       �  �  t	  9   �     �                         � ߱        �	  $  �  H	  ���                       h
  A  �       # 
   ��        
 �	  0�                                         ��   �                   T
  H
           �   �           �  (�         �             
   4
          �  �
         `�      4   ����`�                                      ��                  �  �                  8                       �  �
  �  A  �       $ t   ��         `  ��                                         h�   t�                   �  �           ��  ��           ��  ��         �            �   �    �  A  �       % @  	 ��         (  $�                                         Д   ܔ   �                 �  �      	     ��  �  �      	     ��  �  �         �            \   t      A  �       &   
 ��         �  ��                                         p�   |�                   `  T      
     ��  ��      
     ��  ��         �            ,   @    ؕ                     �                     4�                     ��                     Ԗ       
       
           � ߱            $  �  t  ���                       ��                     �                     �                        � ߱            $     <  ���                                     H                      ��                    
                  �                       �      $   	  t  ���                       T�  @         @�              � ߱        	            �          �  h      ��             	       +  �              ��                �       �      H  �       ��                            7   ����         ��                     �            �                  6             ��                    �            �                                                                T  H                                   @            (   8        O   ����	 	 e�          O   ����	 	 R�          O   ����	 	 ��        $    �  ���                       `�                          � ߱        �  /     4     D                          3   ����t�  t        d                      3   ������  �        �                      3   ������  �        �                      3   ������            �                    3   ������      $     0  ���                                                    � ߱        ��                        � ߱        �  $    \  ���                         $    �  ���                       ܗ                          � ߱        �  /     8     H                          3   �����  x        h                      3   �����  �        �                      3   �����  �        �                      3   ����(�            �                    3   ����,�      $     4  ���                                                    � ߱        8�                        � ߱        �  $    `  ���                         $    �  ���                       X�                          � ߱        �  /     <     L                          3   ����l�  |        l                      3   ������  �        �                      3   ������  �        �                      3   ������            �                    3   ������      $     8  ���                                                    � ߱        ��                        � ߱        �  $    d  ���                         $    �  ���                       Ԙ                          � ߱        �  /     @     P                          3   �����  �        p                      3   �����  �        �                      3   �����  �        �                      3   ���� �                                 3   ����$�      $     <  ���                                                    � ߱        0�                        � ߱        �  $     h  ���                         $  "  �  ���                       P�                          � ߱        �  /   #  D     T                          3   ����d�  �        t                      3   ������  �        �                      3   ������  �        �                      3   ������                                3   ������      $   #  @  ���                                                    � ߱        ��                        � ߱        �  $  %  l  ���                         $  '  �  ���                       ̙                          � ߱        �  /   (  H     X                          3   ������  �        x                      3   ���� �  �        �                      3   �����  �        �                      3   �����                                3   �����      $   (  D  ���                                                    � ߱        (�                        � ߱            $  *  p  ���                           �   -  H�                                                                 ��                              ��        �                   ��                             ��                             ��                            ����                            �  & 
 �  % 	 �  $    #      =                              T          �   l       ��               5  J  �               D=                    O   ����    e�          O   ����    R�          O   ����    ��      �%   '    �              �          �%   '                 �          �%   '    8                      �%   '                   ,         �  $  A  �  ���                       h�      '                   � ߱        �  B  B       (    ��         �  �                                         |�   ��   ��   ��                   |  p           ��  ��  ̚               ��  Ě  Ԛ  ܚ                      8   T          E  �  (      L�      4   ����L�                8                      ��                  E  G                  '                       E  �      $  F  d  ���                       T�      '                   � ߱                    '  �                                     �    '     ��                            ����                                (     )   d d     �	   ��% C&   � �                                               �                                                                         d     D                                                                 P   '	�d                                                           �%  G   
 X  '	xd                                                         �     �     
 X  ��d         d   x                                         �     �      P   '	��d                                                           �%  G   
 X  '	�yd                                                        �     �  
    P   ��Xd                                                           �%  G   
 X  ��?d                                                        �     �  
    P   5	"d                                                            &  G   
 X  5	"�d                                                        �     �     
 X  �"Hd         �   �                                         �     �      \  �
��p                                 y          
       	&                @     
 X   (8d                                                        �     �      P ��� �>                                                        �        D                                                                    TXS appSrvUtils ASSIGNFOCUSEDWIDGET ASSIGNWIDGETVALUE ASSIGNWIDGETVALUELIST BLANKWIDGET CLEARWIDGET DISABLERADIOBUTTON DISABLEWIDGET ENABLERADIOBUTTON ENABLEWIDGET FORMATTEDWIDGETVALUE FORMATTEDWIDGETVALUELIST HIDEWIDGET HIGHLIGHTWIDGET RESETWIDGETVALUE TOGGLEWIDGET VIEWWIDGET WIDGETHANDLE WIDGETLONGCHARVALUE WIDGETISBLANK WIDGETISFOCUSED WIDGETISMODIFIED WIDGETISTRUE WIDGETVALUE WIDGETVALUELIST p-Division s-codcia tt-diferencias tt-codmat tt-DesMat tt-CodFam tt-desFam tt-subFam tt-dessub tt-codmar tt-desmar tt-undstk tt-qty-pedida tt-qty-atendida tt-xatender tt-stk-11s tt-stk-21s tt-stk-11 tt-stk-35 tt-stk-21 tt-stk-38 wWin btnProcesar txtCliente txtCodDiv txtDesClie < Todos los Clientes > txtDesde txtDesDiv txtHasta txtMsg fMain X(11) X(256) 99/99/9999 Consolidado de Articulos por atender - segun cotizaciones GUI Consolidades de Articulos x Atender - COTIZACIONES input-var-1 input-var-2 input-var-3 output-var-1 output-var-2 output-var-3 HANDLE-CAMPO BUTTON-LOOKUP PARIENTE load-imagen program_name program_call titulo-look  ENTRY img/b-lookup FRAME,WINDOW FILL-IN ENTRY CHOOSE INTEGER DECIMAL corre_calculadora DATE corre_calendario BROWSE KEYPRESS qbusca ADM-BUSCA qimprime ADM-IMPRIME campo_name PF-G005 Descripci�n de Campos * _BUSCA-LOOKUP OK-SET-WAIT-STATE GENERAL ? _CORRE-PROGRAM DISABLEPAGESINFOLDER ENABLEPAGESINFOLDER GETCALLERPROCEDURE GETCALLERWINDOW GETCONTAINERMODE GETCONTAINERTARGET GETCONTAINERTARGETEVENTS GETCURRENTPAGE GETDISABLEDADDMODETABS GETDYNAMICSDOPROCEDURE GETFILTERSOURCE GETMULTIINSTANCEACTIVATED GETMULTIINSTANCESUPPORTED GETNAVIGATIONSOURCE GETNAVIGATIONSOURCEEVENTS GETNAVIGATIONTARGET GETOUTMESSAGETARGET GETPAGENTARGET GETPAGESOURCE GETPRIMARYSDOTARGET GETREENABLEDATALINKS GETRUNDOOPTIONS GETRUNMULTIPLE GETSAVEDCONTAINERMODE GETSDOFOREIGNFIELDS GETTOPONLY GETUPDATESOURCE GETUPDATETARGET GETWAITFOROBJECT GETWINDOWTITLEVIEWER GETSTATUSAREA PAGENTARGETS SETCALLEROBJECT SETCALLERPROCEDURE SETCALLERWINDOW SETCONTAINERMODE SETCONTAINERTARGET SETCURRENTPAGE SETDISABLEDADDMODETABS SETDYNAMICSDOPROCEDURE SETFILTERSOURCE SETINMESSAGETARGET SETMULTIINSTANCEACTIVATED SETMULTIINSTANCESUPPORTED SETNAVIGATIONSOURCE SETNAVIGATIONSOURCEEVENTS SETNAVIGATIONTARGET SETOUTMESSAGETARGET SETPAGENTARGET SETPAGESOURCE SETPRIMARYSDOTARGET SETREENABLEDATALINKS SETROUTERTARGET SETRUNDOOPTIONS SETRUNMULTIPLE SETSAVEDCONTAINERMODE SETSDOFOREIGNFIELDS SETTOPONLY SETUPDATESOURCE SETUPDATETARGET SETWAITFOROBJECT SETWINDOWTITLEVIEWER GETOBJECTTYPE SETSTATUSAREA GETALLFIELDHANDLES GETALLFIELDNAMES GETCOL GETDEFAULTLAYOUT GETDISABLEONINIT GETENABLEDOBJFLDS GETENABLEDOBJHDLS GETHEIGHT GETHIDEONINIT GETLAYOUTOPTIONS GETLAYOUTVARIABLE GETOBJECTENABLED GETOBJECTLAYOUT GETROW GETWIDTH GETRESIZEHORIZONTAL GETRESIZEVERTICAL SETALLFIELDHANDLES SETALLFIELDNAMES SETDEFAULTLAYOUT SETDISABLEONINIT SETHIDEONINIT SETLAYOUTOPTIONS SETOBJECTLAYOUT SETRESIZEHORIZONTAL SETRESIZEVERTICAL GETOBJECTTRANSLATED GETOBJECTSECURED CREATEUIEVENTS GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE , prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartWindow ContainerType WINDOW PropertyDialog adm2/support/visuald.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout SupportedLinks Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext ObjectLayout LayoutOptions ObjectEnabled LayoutVariable DefaultLayout DisableOnInit EnabledObjFlds EnabledObjHdls FieldSecurity SecuredTokens AllFieldHandles AllFieldNames MinHeight MinWidth ResizeHorizontal ResizeVertical ObjectSecured ObjectTranslated PopupButtonsInFields ColorInfoBG INTEGER ColorInfoFG ColorWarnBG ColorWarnFG ColorErrorBG ColorErrorFG BGColor FGColor FieldPopupMapping CurrentPage PendingPage ContainerTarget ContainerTargetEvents exitObject,okObject,cancelObject,updateActive ContainerToolbarSource ContainerToolbarSourceEvents toolbar,okObject,cancelObject OutMessageTarget PageNTarget PageSource FilterSource UpdateSource UpdateTarget CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState StartPage RunMultiple WaitForObject DynamicSDOProcedure adm2/dyndata.w RunDOOptions InitialPageList WindowFrameHandle Page0LayoutManager MultiInstanceSupported MultiInstanceActivated ContainerMode SavedContainerMode SdoForeignFields NavigationSource NavigationTarget PrimarySdoTarget NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter CallerWindow CallerProcedure CallerObject DisabledAddModeTabs ReEnableDataLinks WindowTitleViewer UpdateActive InstanceNames ClientNames ContainedDataObjects ContainedAppServices DataContainer HasDbAwareObjects HasDynamicProxy HideOnClose HideChildContainersOnClose HasObjectMenu RequiredPages RemoveMenuOnHide ProcessList PageLayoutInfo PageTokens DataContainerName WidgetIDFileName ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService cFields adm2/visual.p   txtCliente txtDesde txtHasta btnProcesar CTRL-PAGE-UP CTRL-PAGE-DOWN adm2/containr.p Add initializeDataObjects getSupportedLinks data-target data-source buildDataRequest containertoolbar-target ContainerToolbar-Target CLOSE Fechas estan incorrectas... gn-clie Maestro de Clientes Cliente NO Existe... lCodClie iStartPage ADM-ERROR 99/99/9999 ADM-CREATE-OBJECTS DISABLE_UI ENABLE_UI EXITOBJECT GN-DIVI DIVISIONES < ** DIVISION ERRADA ** > < Todas las divisiones > INITIALIZEOBJECT PROCESA-PARAMETROS RECOGE-PARAMETROS chExcelApplication chWorkbook chWorksheet chWorksheetRange iCount iIndex iColumn cColumn cRange x-signo Generando el Excel... Excel.Application Visible Workbooks Sheets Item b1:b1 Range Font Bold b2:b2 b3:b3 B1 Value Cliente  :    B2 Division :  B3 Fechas   : Desde :       Hasta :  A4:R4 A4 CODIGO B4 Descrpcion Articulo C4 Familia D4 SubFamilia E4 Marca F4 U.Medida G4 Cant.Pedida H4 Cant.Atendida I4 Cant.x Atender J4 '11S K4 '21S L4 Total 11S + 21S M4 '11 N4 '35 O4 '21 P4 '38 Q4 Total CDs A ' B C D E F G H I J K L M N O P Q DisplayAlerts Proceso Terminado UM-EXCEL lDife lStk Procesando Cotizaciones.... FacCPedi Pedidos al Credito COT FacDPedi Detalle pedido credito Almmmatg Cat�logo de Materiales Almtfami Tabla de  Familias AlmSFami Tabla de Subfamilias almtabla Tablas de Almacen MK Calculando Saldos de Almacenes... 11S 21S 11 35 21 38 UM-PROCESAR p-almacen p-codmat p-fecha p-Stock AlmStkal AlmStkal UM-SALDOS-ALMACEN idx01 De algun, Cliente Desde Hasta Division Procesar IDX01 llave01 Matg01 fami01 sfam01 tabl01 0  \-  `  44      & �    H                                         �  �  �     �                                         �  �  T   �                                             �   �                                                    !  �   <                                        $  %  &  '    |                                        8  9  L  �                    �                  adm-busca   E  F  �                      �                  adm-imprime Q  R  0        $        campo_name  X        H        program_call              p        program_name    �  �     	             �                  _busca-lookup   a  d  e  h  i  j  m  n  r  u  w                  OK-SET-WAIT-STATE   �  T     
   �          D                  _corre-program  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ��      �        pcProp      ��      �        pcProp      ��              plCancel    @  ��      4        pcProcName  d  ��      X       
 pcProcName  �  ��      |        pcProcName      ��      �       
 pcProcName      ��      �        piPageNum       ��      �        piPageNum       ��              pcPageList      ��      0        pcProc  \  ��      P        pcLinkName      ��      t        pcLinkName  �  ��      �       
 phTarget        ��      �        phTarget        ��      �        piPageNum       ��              pcValue     ��      $        piPageNum       ��      H        pcAction        ��      l       
 phAppService        ��      �        pcMode  �  ��      �       
 phSource    �  ��      �        phSource        ��      �       
 phSource    (  ��               pcText  H  ��      @        pcText      ��      `        pcText  �  ��      �       
 phObject    �  ��      �       
 phObject        ��      �        phObject        ��      �        pcField     ��              pcCursor    <  ��      0       
 phCaller    `  ��      T        phCaller    �  ��      x        phCaller        ��      �        phCaller    �  ��      �        pcMod   �  ��      �        pcMod       ��       	       
 pcMod   ,	  ��       	       
 phSource    P	  ��      D	        phSource        ��      h	       
 phSource    �	  ��      �	        pdRow       ��      �	        pdRow       ��      �	       
 hTarget �	  ��      �	        pcMessage       ��      
        pcMessage       ��      4
        plEnabled             T
     cType     �
     @   @
          �
                  getObjectType   �
  �
  �
  �
        �
  
   hReposBuffer    �
        �
  
   hPropTable             
   hBuffer             
   hTable  \
  d     A   �
          T                  adm-clone-props �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �            �  
   hProc             �        pcProcName  $  ,  	   B   �  �                        start-super-proc    �  �  �  �  �  �  �    
  �  �     C                                   �  P  �     D                                   �  �  �  �     E                                   �  �  �  $     F                                     �  X     G                                       (  �     H                                         `  �     I                                                    !  "  #  $  &  '  )                 lCodClie    �  X     J                                 5  7  8  9  ;  <  =  >  ?  @  C  (  �     K                                   g  h  i  j  �  �     L                                   p  q  �  @     M               ,                  adm-create-objects  �  �  �     N               t                  disable_UI  �  �  �  �  D  �     O               �                  enable_UI   �  �  �  �  �       P                                 exitObject  �  �  �  �  h     Q               T                  initializeObject    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  $  �     R               �                  procesa-parametros          �  <     S               (                  recoge-parametros   '  (  /  2  t        `     chExcelApplication  �        �     chWorkbook  �        �     chWorksheet �        �     chWorksheetRange    �        �     iCount               iIndex  0        (     iColumn L     	   D     cColumn h     
   `     cRange            |     x-signo �  �  R   T   L          �                  um-excel    L  M  N  O  R  U  X  [  _  `  a  g  h  j  l  n  o  p  q  r  s  t  u  w  x  y  {  |  }  ~    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  $              lDife              8     lStk    �  |  4   U             p                  um-procesar �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �               	  
                             "  #  %  '  (  *  +  -  /  p  '      d        p-almacen   �  '      �        p-codmat    �  '      �        p-fecha     '      �        p-Stock @       V       L                        um-saldos-almacen   A  B  E  F  G  J  �  �       ! 8  \  �                          p  �     tt-diferencias  X         d         p         |         �         �         �         �         �         �         �         �         �         �                                     ,         tt-codmat   tt-DesMat   tt-CodFam   tt-desFam   tt-subFam   tt-dessub   tt-codmar   tt-desmar   tt-undstk   tt-qty-pedida   tt-qty-atendida tt-xatender tt-stk-11s  tt-stk-21s  tt-stk-11   tt-stk-35   tt-stk-21   tt-stk-38   X          L  
   appSrvUtils x        l     s-codcia    �       �  
   wWin    �       �     txtCliente  �       �     txtCodDiv   �       �     txtDesClie              txtDesde    4       (     txtDesDiv   T    	   H     txtHasta    p    
   h     txtMsg  �       �     input-var-1 �       �     input-var-2 �       �     input-var-3 �       �     output-var-1                output-var-2    <    	   ,     output-var-3    `    
   P  
   HANDLE-CAMPO    �       t  
   BUTTON-LOOKUP   �       �  
   PARIENTE    �       �     load-imagen �       �     program_name           �     program_call    ,             titulo-look T        @  
   gshAstraAppserver   |  	      h  
   gshSessionManager   �  
      �  
   gshRIManager    �   	     �  
   gshSecurityManager  �   
     �  
   gshProfileManager             
   gshRepositoryManager    H        0  
   gshTranslationManager   l        \  
   gshWebManager   �        �     gscSessionId    �        �     gsdSessionObj   �        �  
   gshFinManager   �        �  
   gshGenManager              
   gshAgnManager   D        4     gsdTempUniqueID d        X     gsdUserObj  �        x     gsdRenderTypeObj    �        �     gsdSessionScopeObj  �       �  
   ghProp  �       �  
   ghADMProps           
   ghADMPropsBuf   <       (     glADMLoadFromRepos  X       P     glADMOk x       l  
   ghContainer �       �     cObjectName �       �     iStart  �       �     cAppService �       �     cASDivision              cServerOperatingMode    <       4     cFields          P     iStartPage           t        p-Division  �    L  �  tt-diferencias  �       �  PF-G005 �       �  gn-clie �       �  GN-DIVI     !    �  FacCPedi         "       FacDPedi    <    #    0   Almmmatg    X    $    L   Almtfami    t    %    h   AlmSFami    �    &    �   almtabla         (    �   AlmStkal             7   g  h  y  �  �  �      #  7  �  �  �  �  �  �  �  S	  T	  U	  V	  m	  y	  z	  {	  }	  	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  J
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
  F  Q  R  T  U  V  W  X  Y  Z  [  \  ]  ^  _  `  a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �                	  
                                             !  "  #  $  %  &  '  (  )  *  +  ,  -  �  �  �  �  �  �  �  �  �  �  �      1  V  r  t  �    )  *  D  T  U  V  Y  Z  [  b  c  �  �  �  I  J  N  X  �  �  �  �  �  �  �  �  �  �                    "  �  �  �  �  �  	    1  ^  _  e  o  t  {    �  �  �  �  �  �  �  �      H� % C:\Progress\OpenEdge\src\adm2\windowmn.i �$  f!  C:\Progress\OpenEdge\src\adm2\containr.i %  � $ %C:\Progress\OpenEdge\src\adm2\custom\containrcustom.i    L%  ��  C:\Progress\OpenEdge\src\adm2\visual.i   �%  # # %C:\Progress\OpenEdge\src\adm2\custom\visualcustom.i  �%  �<  C:\Progress\OpenEdge\src\adm2\appserver.i    &  �� " %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   <&  I�  C:\Progress\OpenEdge\src\adm2\smart.i    �&  Ds ! C:\Progress\OpenEdge\gui\fn  �&  tw   %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   �&  Q.  C:\Progress\OpenEdge\gui\set '  ��  C:\Progress\OpenEdge\src\adm2\cntnprop.i D'  ��  %C:\Progress\OpenEdge\src\adm2\custom\cntnpropcustom.i    x'  P  %C:\Progress\OpenEdge\src\adm2\custom\cntnprtocustom.i    �'  F>  C:\Progress\OpenEdge\src\adm2\visprop.i   (  �I  %C:\Progress\OpenEdge\src\adm2\custom\vispropcustom.i 4(  ��  %C:\Progress\OpenEdge\src\adm2\custom\visprtocustom.i t(  �l  C:\Progress\OpenEdge\src\adm2\appsprop.i �(  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    �(  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    ,)  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i p)  �j  C:\Progress\OpenEdge\gui\get �)  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    �)  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    *  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i T*  Su  C:\Progress\OpenEdge\src\adm2\globals.i  �*  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i �*  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   �*  �  C:\Progress\OpenEdge\src\adm2\appsprto.i @+  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   t+  �X 
 C:\Progress\OpenEdge\src\adm2\visprto.i  �+  !� 	 %C:\Progress\OpenEdge\src\adm2\custom\visualdefscustom.i  �+  n�  C:\Progress\OpenEdge\src\adm2\cntnprto.i 4,  ;  %C:\Progress\OpenEdge\src\adm2\custom\containrdefscustom.i    h,  ��  %d:\newsie\on_in_co\src\adm-vm\method\vmviewer.i  �,  ~�  C:\Progress\OpenEdge\src\adm2\widgetprto.i   �,  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   $-  lP   d:\newsie\on_in_co\APLIC\vta2\w-consolidado-cot-x-atender.w      D  �      �-     R  %   �-  �   �      �-  �   �     �-     �     �-  �   �     �-     �     .  �   �     .     '  $   $.  �        4.       !   D.  �        T.       !   d.  �        t.       !   �.  r   �     �.  n   �     �.     w  #   �.  i   r     �.     P     �.  P   7     �.  �   .     �.     �  "   /  �   �     /     �     $/  �   �     4/     �     D/  �   �     T/     h     d/  g   N     t/     /     �/  O        �/  �   �     �/     �  !   �/  �   o     �/           �/  �        �/     �     �/  �   �     0     �     0  �   �     $0     �     40  �   �     D0     �     T0  �   p     d0     N     t0  �   K     �0     )     �0  }        �0     �     �0          �0     1     �0     �     �0  7   �     �0  �   �     1  O   �     1          $1     1     41  �   �     D1  �   �     T1  O   �     d1     �     t1     s     �1  �   N     �1  x   F     �1  M   1     �1           �1     �
     �1  a   �
     �1  �  �
     �1     }
     2  �  J
     2  O   <
     $2     +
     42     �	     D2  �   	     T2     �     d2     .     t2  x   (     �2          �2     �     �2     �     �2     �     �2     g     �2  Q   W     �2     �     �2     �     3     �     3     �     $3  f   l     43       
   D3  "   �     T3     �  	   d3     �     t3  Z   A     �3     I     �3     
     �3     �     �3     �     �3     �     �3  �   �      �3     �     �3  '   �       4     @      4            $4           