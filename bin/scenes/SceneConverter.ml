let get_named_handler (id : SceneEnumerator.t) =
  match id with
  | MainScene -> ((module MainScene : SceneSign.S), "gallery")
  | GraveyardScene -> ((module GraveyardScene : SceneSign.S), "graveyard")
