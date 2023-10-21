"! <p class="shorttext synchronized" lang="en">CA-TBX: Constants + value checks for text preparation</p>
CLASS zcl_ca_c_text_preparation DEFINITION PUBLIC
                                           CREATE PROTECTED.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Target preparation type</p>
      BEGIN OF preparation_type,
        "! <p class="shorttext synchronized" lang="en">Target preparation type: HTML</p>
        html TYPE zca_d_target_preparation_type VALUE 'HTML'  ##no_text,
        "! <p class="shorttext synchronized" lang="en">Target preparation type: RAW (char. output like old lists)</p>
        raw  TYPE zca_d_target_preparation_type VALUE 'RAW'  ##no_text,
      END OF preparation_type,

      "! <p class="shorttext synchronized" lang="en">Technical additions to control preparation</p>
      BEGIN OF techn_addition,
        "! <p class="shorttext synchronized" lang="en">SAP script command to keep HTML tagging in WI task descr.</p>
        mail_html_command TYPE char10 VALUE 'MAIL_HTML' ##no_text,
      END OF techn_addition,

      "! <p class="shorttext synchronized" lang="en">Constants for HTML preparation</p>
      BEGIN OF html,
        BEGIN OF defaults,
          BEGIN OF font_size,
            "! <p class="shorttext synchronized" lang="en">Default font size: for monospace font = inherit</p>
            monospace    TYPE zca_d_fontsize_monospc VALUE '0'  ##no_text,
            "! <p class="shorttext synchronized" lang="en">Default font size: for proportional font = inherit</p>
            proportional TYPE zca_d_fontsize_prop    VALUE '0'  ##no_text,
          END   OF font_size,

          BEGIN OF font,
            "! <p class="shorttext synchronized" lang="en">Default font: for text</p>
            monospace    TYPE zca_d_font_monospc     VALUE 'Courier New'  ##no_text,
            "! <p class="shorttext synchronized" lang="en">Default font: for HTML</p>
            proportional TYPE zca_d_font_prop        VALUE 'Arial'  ##no_text,
          END   OF font,

          BEGIN OF font_type,
            "! <p class="shorttext synchronized" lang="en">Font type: Monospace</p>
            monospace    TYPE zca_d_font_type        VALUE 'M'  ##no_text,
            "! <p class="shorttext synchronized" lang="en">Font type: Proportional</p>
            proportional TYPE zca_d_font_type        VALUE 'P'  ##no_text,
          END   OF font_type,
        END OF defaults,

        BEGIN OF alignment,
          "! <p class="shorttext synchronized" lang="en">Cell alignment: Inherit</p>
          center  TYPE zca_d_table_desc_alignment VALUE '0'  ##no_text,
          "! <p class="shorttext synchronized" lang="en">Cell alignment: Left</p>
          left    TYPE zca_d_table_desc_alignment VALUE '1'  ##no_text,
          "! <p class="shorttext synchronized" lang="en">Cell alignment: Right</p>
          right   TYPE zca_d_table_desc_alignment VALUE '2'  ##no_text,
          "! <p class="shorttext synchronized" lang="en">Cell alignment: Inherit</p>
          inherit TYPE zca_d_table_desc_alignment VALUE '9'  ##no_text,
        END   OF alignment,

        BEGIN OF frame_style,
          "! <p class="shorttext synchronized" lang="en">Frame style: None (no frame/border)</p>
          none    TYPE zca_d_frame_style      VALUE '0'  ##no_text,
          "! <p class="shorttext synchronized" lang="en">Frame style: Solid</p>
          solid   TYPE zca_d_frame_style      VALUE '1'  ##no_text,
          "! <p class="shorttext synchronized" lang="en">Frame style: Ridge (like 3D)</p>
          ridge   TYPE zca_d_frame_style      VALUE '2'  ##no_text,
          "! <p class="shorttext synchronized" lang="en">Frame style: Inherit from a higher element</p>
          inherit TYPE zca_d_frame_style      VALUE '9'  ##no_text,
        END   OF frame_style,

        BEGIN OF frame_width,
          "! <p class="shorttext synchronized" lang="en">Frame width: Thin</p>
          thin    TYPE zca_d_frame_width      VALUE '0'  ##no_text,
          "! <p class="shorttext synchronized" lang="en">Frame width: Medium</p>
          medium  TYPE zca_d_frame_width      VALUE '1'  ##no_text,
          "! <p class="shorttext synchronized" lang="en">Frame width: Thick</p>
          thick   TYPE zca_d_frame_width      VALUE '2'  ##no_text,
          "! <p class="shorttext synchronized" lang="en">Frame width: Inherit from a higher element</p>
          inherit TYPE zca_d_frame_width      VALUE '9'  ##no_text,
        END   OF frame_width,
      END OF html,

      "! <p class="shorttext synchronized" lang="en">HTML tags</p>
      BEGIN OF html_tag,
        BEGIN OF html,
          open  TYPE its_tag VALUE '<html>'  ##no_text,
          close TYPE its_tag VALUE '</html>'  ##no_text,
        END   OF html,

        BEGIN OF head,
          open  TYPE its_tag VALUE '<head>'  ##no_text,
          close TYPE its_tag VALUE '</head>'  ##no_text,
        END   OF head,

        BEGIN OF body,
          open  TYPE its_tag VALUE '<body>'  ##no_text,
          close TYPE its_tag VALUE '</body>'  ##no_text,
        END   OF body,

        BEGIN OF font,
          open  TYPE its_tag VALUE '<font face="&1,&2" size="&3">'  ##no_text,
          close TYPE its_tag VALUE '</font>'  ##no_text,
        END   OF font,

        "! <p class="shorttext synchronized" lang="en">Grouping element without semantic meaning</p>
        BEGIN OF div,
          open  TYPE its_tag VALUE '<div>'  ##no_text,
          close TYPE its_tag VALUE '</div>'  ##no_text,
        END   OF div,

        BEGIN OF table_row,
          open  TYPE its_tag VALUE '<tr>'  ##no_text,
          close TYPE its_tag VALUE '</tr>'  ##no_text,
        END   OF table_row,

        BEGIN OF data_column,
          open  TYPE its_tag VALUE '<td>'  ##no_text,
          close TYPE its_tag VALUE '</td>'  ##no_text,
        END   OF data_column,

        line_break TYPE its_tag VALUE '<br>'  ##no_text,
      END   OF html_tag.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Get instance</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">Class instance</p>
      get_instance
        RETURNING
          VALUE(result) TYPE REF TO zcl_ca_c_text_preparation.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Get technical description of value reference</p>
      "!
      "! @parameter value_ref               | <p class="shorttext synchronized" lang="en">Data reference of value</p>
      "! @parameter result                  | <p class="shorttext synchronized" lang="en">Technical description to passed value</p>
      "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
      get_technical_description
        IMPORTING
          value_ref     TYPE REF TO data
        RETURNING
          VALUE(result) TYPE REF TO cl_abap_datadescr
        RAISING
          zcx_ca_text_preparation,

      "! <p class="shorttext synchronized" lang="en">Valid alignment passed?</p>
      "!
      "! @parameter alignment               | <p class="shorttext synchronized" lang="en">Alignment</p>
      "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
      is_alignment_valid
        IMPORTING
          alignment TYPE zca_d_table_desc_alignment
        RAISING
          zcx_ca_text_preparation,

      "! <p class="shorttext synchronized" lang="en">Valid frame style passed?</p>
      "!
      "! @parameter font_size               | <p class="shorttext synchronized" lang="en">Font size</p>
      "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
      is_font_size_valid
        IMPORTING
          font_size TYPE zca_d_fontsize_prop
        RAISING
          zcx_ca_text_preparation,

      "! <p class="shorttext synchronized" lang="en">Valid frame style passed?</p>
      "!
      "! @parameter frame_style             | <p class="shorttext synchronized" lang="en">Frame style</p>
      "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
      is_frame_style_valid
        IMPORTING
          frame_style TYPE zca_d_frame_style
        RAISING
          zcx_ca_text_preparation,

      "! <p class="shorttext synchronized" lang="en">Valid frame width passed?</p>
      "!
      "! @parameter frame_width             | <p class="shorttext synchronized" lang="en">Frame width</p>
      "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
      is_frame_width_valid
        IMPORTING
          frame_width TYPE zca_d_frame_width
        RAISING
          zcx_ca_text_preparation,

      "! <p class="shorttext synchronized" lang="en">Valid target preparation type passed?</p>
      "!
      "! @parameter preparation_type        | <p class="shorttext synchronized" lang="en">Target preparation type</p>
      "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
      is_preparation_type_valid
        IMPORTING
          preparation_type TYPE zca_d_target_preparation_type
        RAISING
          zcx_ca_text_preparation.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Check value against fixed_values</p>
      "!
      "! @parameter value                   | <p class="shorttext synchronized" lang="en">Value under test</p>
      "! @parameter param_name              | <p class="shorttext synchronized" lang="en">Name of field/parameter for output in error message</p>
      "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
      check_against_fixed_values
        IMPORTING
          value      TYPE simple
          param_name TYPE csequence
        RAISING
          zcx_ca_text_preparation,

      "! <p class="shorttext synchronized" lang="en">Check value against fixed_values</p>
      "!
      "! @parameter value                   | <p class="shorttext synchronized" lang="en">Value under test</p>
      "! @parameter name                    | <p class="shorttext synchronized" lang="en">Name of field/parameter for output in error message</p>
      "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
      is_html_control_value_valid
        IMPORTING
          value TYPE numc1
          name  TYPE syst_msgv
        RAISING
          zcx_ca_text_preparation.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Instance of the class itself</p>
      singleton_instance     TYPE REF TO zcl_ca_c_text_preparation.

ENDCLASS.



CLASS zcl_ca_c_text_preparation IMPLEMENTATION.

  METHOD check_against_fixed_values.
    "-----------------------------------------------------------------*
    "   Check value against fixed_values
    "-----------------------------------------------------------------*
    TRY.
        NEW zcl_ca_ddic( iv_data       = value
                         iv_param_name = param_name
                                  )->check_fixed_values( iv_value       = value
                                                         iv_raise_excep = abap_true ).

      CATCH zcx_ca_param INTO DATA(lx_catched).
        DATA(lx_error) = CAST zcx_ca_text_preparation( lx_catched ).
        RAISE EXCEPTION lx_error.
    ENDTRY.
  ENDMETHOD.                    "check_against_fixed_values


  METHOD get_instance.
    "-----------------------------------------------------------------*
    "   Get instance
    "-----------------------------------------------------------------*
    IF zcl_ca_c_text_preparation=>singleton_instance IS NOT BOUND.
      zcl_ca_c_text_preparation=>singleton_instance = NEW #( ).
    ENDIF.

    result = zcl_ca_c_text_preparation=>singleton_instance.
  ENDMETHOD.                    "get_instance


  METHOD get_technical_description.
    "-----------------------------------------------------------------*
    "   Get technical description of value reference
    "-----------------------------------------------------------------*
    TRY.
        result ?= NEW zcl_ca_ddic( ir_data = value_ref )->mo_type_desc.

      CATCH zcx_ca_error
            cx_sy_move_cast_error INTO DATA(_catched).
        DATA(_exception) = CAST zcx_ca_text_preparation(
                                      zcx_ca_error=>create_exception(
                                                   iv_excp_cls = zcx_ca_text_preparation=>c_zcx_ca_text_preparation
                                                   ix_error    = _catched ) ) ##no_text.
        IF _exception IS BOUND.
          RAISE EXCEPTION _exception.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "get_technical_description


  METHOD is_alignment_valid.
    "-----------------------------------------------------------------*
    "   Valid alignment passed?
    "-----------------------------------------------------------------*
    is_html_control_value_valid( value = alignment
                                 name  = 'Alignment'(ali) ) ##no_text.
  ENDMETHOD.                    "is_alignment_valid


  METHOD is_font_size_valid.
    "-----------------------------------------------------------------*
    "   Valid font size passed?
    "-----------------------------------------------------------------*
    IF font_size NOT BETWEEN '0' AND '7'.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_text_preparation
        EXPORTING
          textid   = zcx_ca_text_preparation=>param_invalid
          mv_msgty = zcx_ca_text_preparation=>c_msgty_e
          mv_msgv1 = 'FONT_SIZE'
          mv_msgv2 = CONV #( font_size ).
    ENDIF.
  ENDMETHOD.                    "is_font_size_valid


  METHOD is_frame_style_valid.
    "-----------------------------------------------------------------*
    "   Valid frame style passed?
    "-----------------------------------------------------------------*
    is_html_control_value_valid( value = frame_style
                                 name  = 'Frame style'(fst) ) ##no_text.
  ENDMETHOD.                    "is_frame_style_valid


  METHOD is_frame_width_valid.
    "-----------------------------------------------------------------*
    "   Valid frame width passed?
    "-----------------------------------------------------------------*
    is_html_control_value_valid( value = frame_width
                                 name  = 'Frame width'(fwi) ) ##no_text.
  ENDMETHOD.                    "is_frame_width_valid


  METHOD is_html_control_value_valid.
    "-----------------------------------------------------------------*
    "   Valid frame style passed?
    "-----------------------------------------------------------------*
    IF value CN '0129'.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_text_preparation
        EXPORTING
          textid   = zcx_ca_text_preparation=>param_invalid
          mv_msgty = zcx_ca_text_preparation=>c_msgty_e
          mv_msgv1 = name
          mv_msgv2 = CONV #( value ).
    ENDIF.
  ENDMETHOD.                    "is_html_control_value_valid


  METHOD is_preparation_type_valid.
    "-----------------------------------------------------------------*
    "   Valid target preparation type passed?
    "-----------------------------------------------------------------*
    check_against_fixed_values( value      = preparation_type
                                param_name = 'PREPARATION_TYPE' ) ##no_text.
  ENDMETHOD.                    "is_preparation_type_valid

ENDCLASS.
