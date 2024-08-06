open! Core
open! Async_kernel
open! Import

module App = struct
  module Model = struct
    type t = { text_buffer : string } [@@deriving compare]

    let cutoff = [%compare.equal: t]
  end

  module State = struct
    type t = unit
  end

  module Action = struct
    type t = Text_buffer_update of string [@@deriving sexp]
  end

  let on_startup ~schedule_action:_ _model = Deferred.unit

  let parse input =
    let lexbuf = Lexing.from_string ~with_positions:true input in
    (* TODO: how do we get better errors here...? *)
    Result.try_with (fun () -> Parse.implementation lexbuf)
  ;;

  let take_full_size =
    [ Vdom.Attr.style (Css_gen.width (`Percent Percent.one_hundred_percent))
    ; Vdom.Attr.style (Css_gen.height (`Percent Percent.one_hundred_percent))
    ]
  ;;

  let readonly = [ Vdom.Attr.readonly; Vdom.Attr.disabled ]

  let parse_result_view parse_result ~f =
    let open Vdom.Node in
    let text_content, attrs =
      match parse_result with
      | Ok structure -> f structure, []
      | Error exn ->
        ( [%string "parsing failed: %{exn#Exn}"]
        , List.map [ Css_gen.color (`Name "red"); Css_gen.bold ] ~f:Vdom.Attr.style )
    in
    textarea ~attrs:(attrs @ take_full_size @ readonly) [ text text_content ]
  ;;

  let create (model : Model.t Incr.t) ~old_model:_ ~inject =
    let%map.Incr ({ text_buffer } as model) = model in
    let pprint_ast_view =
      parse text_buffer |> parse_result_view ~f:Pprintast.string_of_structure
    in
    let sexp_ast_view =
      parse text_buffer
      |> parse_result_view ~f:(fun structure ->
        [%sexp_of: Ast_with_derivers.structure] structure |> Sexp.to_string_hum)
    in
    let flex_container ?(attrs = []) contents ~direction =
      let open Vdom.Node in
      div
        ~attrs:(Vdom.Attr.style (Css_gen.flex_container ~direction ()) :: attrs)
        contents
    in
    let flex_row = flex_container ~direction:`Row in
    let flex_column = flex_container ~direction:`Column in
    let view =
      let open Vdom.Node in
      flex_row
        ~attrs:take_full_size
        [ flex_column
            ~attrs:
              (Vdom.Attr.on_input (fun _ev updated_text_buffer ->
                 inject (Action.Text_buffer_update updated_text_buffer))
               :: take_full_size)
            [ textarea ~attrs:take_full_size [ text text_buffer ] ]
        ; flex_column
            ~attrs:take_full_size
            (* TODO: Disable views here*)
            [ pprint_ast_view; sexp_ast_view ]
        ]
    in
    Component.create
      ~apply_action:(fun (action : Action.t) () ~schedule_action:_ : Model.t ->
        match action with
        | Text_buffer_update updated_text_buffer -> { text_buffer = updated_text_buffer })
      model
      view
  ;;
end

let () =
  Start_app.start
    ~bind_to_element_with_id:"app"
    ~initial_model:
      ({ text_buffer = [%string {|open! Core

let () = print_endline "Hello, world!"|}] }
       : App.Model.t)
    (module App)
;;
