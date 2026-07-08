"! <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing a text</p>
CLASS zcx_ca_text_preparation DEFINITION
  PUBLIC
  INHERITING FROM zcx_ca_param
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF zcx_ca_text_preparation,
        msgid TYPE symsgid VALUE 'ZCA_TOOLBOX',
        msgno TYPE symsgno VALUE '022',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ca_text_preparation .
    CONSTANTS:
      BEGIN OF move_to_struc_failed,
        msgid TYPE symsgid VALUE 'ZCA_TOOLBOX',
        msgno TYPE symsgno VALUE '023',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF move_to_struc_failed .
    CONSTANTS:
      BEGIN OF not_a_table,
        msgid TYPE symsgid VALUE 'ZCA_TOOLBOX',
        msgno TYPE symsgno VALUE '024',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF not_a_table .
    CONSTANTS:
      BEGIN OF link_too_long,
        msgid TYPE symsgid VALUE 'ZCA_TOOLBOX',
        msgno TYPE symsgno VALUE '025',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE 'MV_MSGV2',
        attr3 TYPE scx_attrname VALUE 'MV_MSGV3',
        attr4 TYPE scx_attrname VALUE '',
      END OF link_too_long .
    CONSTANTS:
      BEGIN OF descr_too_long,
        msgid TYPE symsgid VALUE 'ZCA_TOOLBOX',
        msgno TYPE symsgno VALUE '027',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE 'MV_MSGV2',
        attr3 TYPE scx_attrname VALUE 'MV_MSGV3',
        attr4 TYPE scx_attrname VALUE 'MV_MSGV4',
      END OF descr_too_long .
    CONSTANTS:
      BEGIN OF symbol_not_found,
        msgid TYPE symsgid VALUE 'ZCA_TOOLBOX',
        msgno TYPE symsgno VALUE '029',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF symbol_not_found .
    CONSTANTS:
      BEGIN OF no_text_available,
        msgid TYPE symsgid VALUE 'ZCA_TOOLBOX',
        msgno TYPE symsgno VALUE '113',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_text_available .
    CONSTANTS:
      BEGIN OF no_result_due_to_param_combi,
        msgid TYPE symsgid VALUE 'ZCA_TOOLBOX',
        msgno TYPE symsgno VALUE '121',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE 'MV_MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_result_due_to_param_combi .
    CONSTANTS:
      BEGIN OF structure_is_not_appropriate,
        msgid TYPE symsgid VALUE 'ZCA_TOOLBOX',
        msgno TYPE symsgno VALUE '130',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF structure_is_not_appropriate .

    CONSTANTS c_zcx_ca_text_preparation TYPE seoclsname VALUE 'ZCX_CA_TEXT_PREPARATION' ##NO_TEXT.

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        !textid      LIKE if_t100_message=>t100key OPTIONAL
        !previous    LIKE previous OPTIONAL
        !mt_return   TYPE bapiret2_t OPTIONAL
        !mv_subrc    TYPE syst_subrc OPTIONAL
        !mv_msgty    TYPE symsgty OPTIONAL
        !mv_msgv1    TYPE symsgv OPTIONAL
        !mv_msgv2    TYPE symsgv OPTIONAL
        !mv_msgv3    TYPE symsgv OPTIONAL
        !mv_msgv4    TYPE symsgv OPTIONAL
        !mv_severity TYPE t_severity OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_ca_text_preparation IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous    = previous
        mt_return   = mt_return
        mv_subrc    = mv_subrc
        mv_msgty    = mv_msgty
        mv_msgv1    = mv_msgv1
        mv_msgv2    = mv_msgv2
        mv_msgv3    = mv_msgv3
        mv_msgv4    = mv_msgv4
        mv_severity = mv_severity.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = zcx_ca_text_preparation .
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
