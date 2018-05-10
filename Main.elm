--module FrontEnd exposing (..)

-- Set up package, not all are needed -----------------------------------------

--import Random exposing (Generator, Seed)
import CoreVal exposing (..)
import ProbPatternSize exposing (..)
import ProbPatternization exposing (..)
import Numpatsize exposing (..)
import KeySignature exposing (..)
import ProbMR exposing (..)
import ProbCR exposing (..)
import ProbOctave exposing (..)
import ProbOctaveChord exposing (..)
import ProbType exposing (..)
import ProbApplied exposing (..)
import ProbRoot exposing (..)
import ProbAddOn exposing (..)
import SeedAd exposing (..)
import ResultS exposing (..)
--
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput,onClick)
import MusicMakerProject as MMP
import Random exposing (Generator, Seed)
import Keyboard
import Mouse
--import Svg exposing (..)
--import Svg.Attributes exposing (..)


----------------------------------------------------------------------------

main : Program Never Model Msg
main =
    Html.program
      { init  = (initialModel,Cmd.none)
      , view = view
      , update = update
      , subscriptions = \_ -> Sub.none
      }

type alias Model =
  {
    seeds : SeedAd.Model,
    coreval : CoreVal.Model,
    probpatternsize : ProbPatternSize.Model,
    probpatternization : ProbPatternization.Model,
    numpatzise : Numpatsize.Model,
    ksp : KeySignature.Model,
    probmr : ProbMR.Model,
    probcr : ProbCR.Model,
    proboctavemelody : ProbOctave.Model,
    proboctavechord : ProbOctaveChord.Model,
    probtypechord : ProbType.Model,
    probappliechord : ProbApplied.Model,
    probroot : ProbRoot.Model,
    probaddon : ProbAddOn.Model,
    results : ResultS.Model
  }

type Msg
  = CoreValMsg CoreVal.Msg
  | ProbPatSizeMsg ProbPatternSize.Msg
  | ProbPatizeMsg ProbPatternization.Msg
  | NumPatSizeMsg Numpatsize.Msg
  | KSPMsg KeySignature.Msg
  | ProbmrMsg ProbMR.Msg
  | ProbcrMsg ProbCR.Msg
  | ProbOcMelMsg ProbOctave.Msg
  | ProbOcChordMsg ProbOctaveChord.Msg
  | ProbTypeChordMsg ProbType.Msg
  | ProbApChordMsg ProbApplied.Msg
  | ProbRootMsg ProbRoot.Msg
  | ProbAddOnMsg ProbAddOn.Msg
  | ResultSMsg ResultS.Msg
  | SeedAdMsg SeedAd.Msg
  | Randin String
  | Randomize Int
  | Default
  | Generate

initialModel : Model
initialModel =
  {
    seeds = SeedAd.initialModel,
    coreval = CoreVal.initialModel,
    probpatternsize = ProbPatternSize.initialModel,
    probpatternization = ProbPatternization.initialModel,
    numpatzise = Numpatsize.initialModel,
    ksp = KeySignature.initialModel,
    probmr = ProbMR.initialModel,
    probcr = ProbCR.initialModel,
    proboctavemelody = ProbOctave.initialModel,
    proboctavechord = ProbOctaveChord.initialModel,
    probtypechord = ProbType.initialModel,
    probappliechord = ProbApplied.initialModel,
    probroot = ProbRoot.initialModel,
    probaddon = ProbAddOn.initialModel,
    results = ResultS.initialModel
  }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
  CoreValMsg m ->
    let (subMod,subCmd) = CoreVal.update m model.coreval in
    { model | coreval = subMod }
                    ! [ Cmd.map CoreValMsg subCmd ]
  ProbPatSizeMsg m ->
    let (subMod,subCmd) = ProbPatternSize.update m model.probpatternsize in
    { model | probpatternsize = subMod }
                    ! [ Cmd.map ProbPatSizeMsg subCmd ]
  ProbPatizeMsg m ->
    let (subMod,subCmd) = ProbPatternization.update m model.probpatternization in
    { model | probpatternization = subMod }
                    ! [ Cmd.map ProbPatizeMsg subCmd ]
  NumPatSizeMsg m ->
    let (subMod,subCmd) = Numpatsize.update m model.numpatzise in
    { model | numpatzise = subMod }
                    ! [ Cmd.map NumPatSizeMsg subCmd ]
  KSPMsg m ->
    let (subMod,subCmd) = KeySignature.update m model.ksp in
    { model | ksp = subMod }
                    ! [ Cmd.map KSPMsg subCmd ]
  ProbmrMsg m ->
    let (subMod,subCmd) = ProbMR.update m model.probmr in
    { model | probmr = subMod }
                    ! [ Cmd.map ProbmrMsg subCmd ]
  ProbcrMsg m ->
    let (subMod,subCmd) = ProbCR.update m model.probcr in
    { model | probcr = subMod }
                    ! [ Cmd.map ProbcrMsg subCmd ]
  ProbOcMelMsg m ->
    let (subMod,subCmd) = ProbOctave.update m model.proboctavemelody in
    { model | proboctavemelody = subMod }
                    ! [ Cmd.map ProbOcMelMsg subCmd ]
  ProbOcChordMsg m ->
    let (subMod,subCmd) = ProbOctaveChord.update m model.proboctavechord in
    { model | proboctavechord = subMod }
                    ! [ Cmd.map ProbOcChordMsg subCmd ]
  ProbTypeChordMsg m ->
    let (subMod,subCmd) = ProbType.update m model.probtypechord in
    { model | probtypechord = subMod }
                    ! [ Cmd.map ProbTypeChordMsg subCmd ]
  ProbApChordMsg m ->
    let (subMod,subCmd) = ProbApplied.update m model.probappliechord in
    { model | probappliechord = subMod }
                    ! [ Cmd.map ProbApChordMsg subCmd ]
  ProbRootMsg m ->
    let (subMod,subCmd) = ProbRoot.update m model.probroot in
    { model | probroot = subMod }
                    ! [ Cmd.map ProbRootMsg subCmd ]
  ProbAddOnMsg m ->
    let (subMod,subCmd) = ProbAddOn.update m model.probaddon in
    { model | probaddon = subMod }
                    ! [ Cmd.map ProbAddOnMsg subCmd ]
  SeedAdMsg m ->
    let (subMod,subCmd) = SeedAd.update m model.seeds in
    { model | seeds = subMod }
                    ! [ Cmd.map SeedAdMsg subCmd ]
  ResultSMsg m ->
    let (subMod,subCmd) = ResultS.update m model.results in
    { model | results = subMod }
                    ! [ Cmd.map ResultSMsg subCmd ]
  Randomize iseed->
    update (SeedAdMsg (SeedAd.GetSeed iseed)) model |> Tuple.first |>
    update (CoreValMsg (CoreVal.Randomize model.seeds.seedcore)) |> Tuple.first |>
    update (ProbPatSizeMsg (ProbPatternSize.Randomize model.seeds.seedpas)) |> Tuple.first |>
    update (ProbPatizeMsg (ProbPatternization.Randomize model.seeds.seedpap)) |> Tuple.first |>
    update (NumPatSizeMsg (Numpatsize.Randomize model.seeds.seednps)) |> Tuple.first |>
    update (KSPMsg (KeySignature.Randomize model.seeds.seedks)) |> Tuple.first |>
    update (ProbmrMsg (ProbMR.Randomize model.seeds.seedpmr)) |> Tuple.first |>
    update (ProbcrMsg (ProbCR.Randomize model.seeds.seedpcr)) |> Tuple.first |>
    update (ProbOcMelMsg (ProbOctave.Randomize model.seeds.seedpoc)) |> Tuple.first |>
    update (ProbOcChordMsg (ProbOctaveChord.Randomize model.seeds.seedpocc)) |> Tuple.first |>
    update (ProbTypeChordMsg (ProbType.Randomize model.seeds.seedtype)) |> Tuple.first |>
    update (ProbApChordMsg (ProbApplied.Randomize model.seeds.seedpa)) |> Tuple.first |>
    update (ProbRootMsg (ProbRoot.Randomize model.seeds.seedroot)) |> Tuple.first |>
    update (ProbAddOnMsg (ProbAddOn.Randomize model.seeds.seedpao))
  Randin stoint ->
    let is = Result.withDefault 0 (String.toInt stoint) in
    update (Randomize is) model
  Generate ->
    let t =
      MMP.mmk {oc = model.coreval.oc} {ic = model.coreval.ic} model.probpatternsize {prc = model.coreval.prc} model.proboctavemelody model.probaddon model.probroot model.probappliechord model.probtypechord model.probcr model.probmr model.ksp [] {ok = model.coreval.ok} {ik = model.coreval.ik} (model.seeds.ogseed) in
    update (ResultSMsg (ResultS.Set t)) model
  Default ->
    (initialModel,Cmd.none)
--  _ ->
--    (model,Cmd.none)

view : Model -> Html Msg
view model =
  Html.div [] [input [ placeholder "Enter a number as seed", onInput Randin , myStyle ] []
              ,button [ onClick (Randomize 5)] [text "Randomize"]
              ,button [ onClick (Default)] [text "Default"]
              ,Html.p [myStyle] [Html.text (toString model.results)]
              --,Html.div [] [Html.text (toString (ProbPatternSize.getVal model.probpatternsize))]
              ,button [ onClick Generate ] [text "Generate"]
              ]

myStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]
