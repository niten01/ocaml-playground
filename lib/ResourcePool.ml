open Raylib

module type ResourceType = sig
  type t
end

module ResourceCounter (Resource : ResourceType) = struct
  type t = { source : string; resource : Resource.t; counter : int }

  let source this = this.source
  let resource this = this.resource
  let counter this = this.counter
  let increment this = { this with counter = this.counter + 1 }
  let decrement this = { this with counter = this.counter - 1 }
end

module ModelRC = ResourceCounter (Model)

type t = { models : ModelRC.t list }

let instance : t ref = ref { models = [] }

let use file =
  let existing_idx_opt =
    List.find_index (fun mrc -> ModelRC.source mrc = file) !instance.models
  in
  match existing_idx_opt with
  | None ->
      let new_model = load_model file in
      instance :=
        {
          models =
            { source = file; resource = new_model; counter = 1 }
            :: !instance.models;
        };
      new_model
  | Some idx ->
      instance :=
        {
          models =
            List.mapi
              (fun i r -> if i = idx then ModelRC.increment r else r)
              !instance.models;
        };
      ModelRC.resource (List.nth !instance.models idx)

let free file =
  let existing_idx_opt =
    List.find_index (fun m -> ModelRC.source m = file) !instance.models
  in
  match existing_idx_opt with
  | None -> failwith "Trying to free resource that was not allocated"
  | Some idx ->
      let models =
        List.mapi
          (fun i r -> if i = idx then ModelRC.decrement r else r)
          !instance.models
        |> List.filter (fun r ->
               if ModelRC.counter r = 0 then (
                 unload_model @@ ModelRC.resource r;
                 false)
               else true)
      in
      instance := { models }
