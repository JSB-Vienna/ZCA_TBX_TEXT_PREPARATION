"! <p class="shorttext synchronized" lang="en">CA-TBX: Text preparation: Techn. details of used structure</p>
CLASS zcl_ca_text_preparation_struct DEFINITION PUBLIC
                                                FINAL
                                                CREATE PROTECTED
                                                GLOBAL FRIENDS zif_ca_text_preparation_table.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   t y p e   d e f i n i t i o n s
    TYPES:
      "! <p class="shorttext synchronized" lang="en">Combined structure of component and DDIC description</p>
      BEGIN OF ty_s_structure_component,
        name     TYPE string,
        position TYPE i,
        element  TYPE REF TO zcl_ca_text_preparation_elem,
      END   OF ty_s_structure_component,
      "! <p class="shorttext synchronized" lang="en">Table with combined data description</p>
      ty_t_structure_components TYPE HASHED TABLE OF ty_s_structure_component
                                                WITH UNIQUE KEY primary_key     COMPONENTS name
                                                WITH UNIQUE SORTED KEY position COMPONENTS position.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Original techn. structure description</p>
      description TYPE REF TO cl_abap_datadescr READ-ONLY,

*     d a t a   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Dynamically created reference for data row</p>
      table_row   TYPE REF TO data,

*     t a b l e s
      "! <p class="shorttext synchronized" lang="en">Prepared structure components, is changeable for value ref</p>
      components  TYPE ty_t_structure_components READ-ONLY.


*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Get instance</p>
      "!
      "! @parameter row_description         | <p class="shorttext synchronized" lang="en">Technical row description</p>
      "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
      get_instance
        IMPORTING
          row_description TYPE REF TO cl_abap_datadescr
        RETURNING
          VALUE(result)   TYPE REF TO zcl_ca_text_preparation_struct
        RAISING
          zcx_ca_text_preparation.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      "!
      "! @parameter row_description         | <p class="shorttext synchronized" lang="en">Technical row description</p>
      "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
      constructor
        IMPORTING
          row_description TYPE REF TO cl_abap_datadescr
        RAISING
          zcx_ca_text_preparation.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   t y p e   d e f i n i t i o n s
    TYPES:
      "! <p class="shorttext synchronized" lang="en">Technical description_of the structure</p>
      BEGIN OF ty_s_techn_row_descr,
        techn_name    TYPE rs38l_par_,
        structure_def TYPE REF TO zcl_ca_text_preparation_struct,
      END   OF ty_s_techn_row_descr,
      "! <p class="shorttext synchronized" lang="en">Technical descriptions of all structures</p>
      ty_t_techn_row_descrs TYPE HASHED TABLE OF ty_s_techn_row_descr
                                            WITH UNIQUE KEY primary_key COMPONENTS techn_name.

*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     t a b l e s
      "! <p class="shorttext synchronized" lang="en">Technical descriptions of all structures</p>
      buffer_techn_row_descrs TYPE ty_t_techn_row_descrs.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Constants and value checks for text module preparation</p>
      tp_options        TYPE REF TO zcl_ca_c_text_preparation.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Add row description as elementary component to collection</p>
      "!
      "! @parameter structure_component | <p class="shorttext synchronized" lang="en">Structure component including description</p>
      add_as_elementary_component
        IMPORTING
          structure_component TYPE abap_simple_componentdescr
          position            TYPE i
        RAISING
          zcx_ca_text_preparation,

      "! <p class="shorttext synchronized" lang="en">Add row description as structured component to collection</p>
      "!
      "! @parameter row_description | <p class="shorttext synchronized" lang="en">Technical row description</p>
      add_as_structured_component
        IMPORTING
          row_description TYPE REF TO cl_abap_datadescr
        RAISING
          zcx_ca_text_preparation,

      "! <p class="shorttext synchronized" lang="en">Create data reference for a single table row for ASSIGN</p>
      create_data_ref_for_table_row.

ENDCLASS.                     "zcl_ca_text_preparation_struct  DEFINITION



CLASS zcl_ca_text_preparation_struct IMPLEMENTATION.

  METHOD add_as_elementary_component.
    "-----------------------------------------------------------------*
    "   Add row description as elementary component to component
    "   collection completed by DDIC information
    "-----------------------------------------------------------------*
    INSERT VALUE ty_s_structure_component(
                   name     = to_upper( structure_component-name )
                   position = position                 "Important for secondary table key
                   element  = NEW zcl_ca_text_preparation_elem( name_to_value = to_upper( structure_component-name )
                                                                position      = position
                                                                description   = structure_component-type
                                                                table_row     = table_row ) ) INTO TABLE components.
  ENDMETHOD.                    "add_as_elementary_component


  METHOD add_as_structured_component.
    "-----------------------------------------------------------------*
    "   Add row description as elementary component to component
    "   collection completed by DDIC information, if possible
    "-----------------------------------------------------------------*
    "Downcast into structure description and get components inclusive resolved includes as deep as possible
    DATA(_structure_components) = CAST cl_abap_structdescr( row_description )->get_included_view( 9 ).

    "Get components of the structure - LO_STRU_DESC supplies no CURR/QUAN reference field
    "Structure is  N O T  in DDIC defined -> consider only elementary columns
    DATA(_position) = 0.
    LOOP AT _structure_components INTO DATA(_structure_component)
                                  WHERE type->kind EQ cl_abap_datadescr=>kind_elem.
      _position += 1.
      add_as_elementary_component( structure_component = _structure_component
                                   position            = _position ).
    ENDLOOP.
  ENDMETHOD.                    "add_as_structured_component


  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    TRY.
        tp_options  = zcl_ca_c_text_preparation=>get_instance( ).
        description = row_description.
        create_data_ref_for_table_row( ).

        CASE row_description->kind.
          WHEN row_description->kind_elem.
            "Table is defined of a single data element + has therefore no components
            add_as_elementary_component( structure_component = VALUE #( "name = `TABLE_LINE` ##no_text
                                                                        type = CAST #( row_description ) )
                                         position            = 1 ).

          WHEN row_description->kind_struct.
            add_as_structured_component( row_description ).
        ENDCASE.

      CATCH cx_sy_move_cast_error INTO DATA(_catched).
        DATA(_exception) = CAST zcx_ca_text_preparation(
                                     zcx_ca_error=>create_exception(
                                                 iv_excp_cls = zcx_ca_text_preparation=>c_zcx_ca_text_preparation
                                                 ix_error    = _catched ) ) ##no_text.
        IF _exception IS BOUND.
          RAISE EXCEPTION _exception.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "constructor


  METHOD create_data_ref_for_table_row.
    "-----------------------------------------------------------------*
    "   Create data reference for a single table row for ASSIGN
    "-----------------------------------------------------------------*
    TRY.
        "Create a workarea for the data table on base of the RTTI line description
        CREATE DATA table_row TYPE HANDLE description.

      CATCH cx_sy_create_data_error INTO DATA(_catched).
        "Error occurred while creating a data object
        RAISE EXCEPTION TYPE zcx_ca_intern
          EXPORTING
            textid   = zcx_ca_intern=>data_creation_failed
            previous = _catched.
    ENDTRY.
  ENDMETHOD.                    "create_data_ref_for_table_row


  METHOD get_instance.
    "-----------------------------------------------------------------*
    "   Get instance
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      _buffer_techn_row_descr TYPE ty_s_techn_row_descr.

    TRY.
        _buffer_techn_row_descr-techn_name = row_description->get_relative_name( ).
        result = buffer_techn_row_descrs[ techn_name = _buffer_techn_row_descr-techn_name ]-structure_def.

      CATCH cx_sy_itab_line_not_found.
        result = _buffer_techn_row_descr-structure_def = NEW #( row_description ).
        INSERT _buffer_techn_row_descr INTO TABLE buffer_techn_row_descrs.
    ENDTRY.
  ENDMETHOD.                    "get_instance

ENDCLASS.                     "zcl_ca_text_preparation_struct  IMPLEMENTATION


