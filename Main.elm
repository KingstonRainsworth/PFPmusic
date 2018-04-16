module FrontEnd exposing (..)

-- Set up package, not all are needed -----------------------------------------

--import Random exposing (Generator, Seed)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput,onClick)
import MusicMakerProject as MMP
import Keyboard
import Mouse
--import Svg exposing (..)
--import Svg.Attributes exposing (..)


----------------------------------------------------------------------------

--main : Program Never Model Msg
main =
    Html.beginnerProgram
      { model  = initialModel
      , view = view
      , update = update
      --, subscriptions = subscriptions
      }

type alias Model =
  {
    coreval : CoreVal.Model,
    probpatternsize = ProbPatternSize.Model,
    probpatternization = ProbPatternization.Model,
    numpatzise = Numpatsize.Model
    ksp : KeySignature.Model,
    probmr : ProbMR.Model,
    probcr : ProbCR.Model,
    proboctavemelody : ProbOctave.Model,
    proboctavechord : ProbOctaveChord.Model,
    probtypechord : ProbType.Model,
    probappliechord : ProbApplied.Model,
    probroot : ProbRoot.Model
    probaddon : ProbAddOn.Model
  }

type Msg
  = CoreValMsg Coreval.Msg
  | ProbPatSizeMsg ProbPatternSize.Msg
  | ProbPatizeMsg = ProbPatternization.Msg,
  | NumPatSizeMsg = Numpatsize.Msg
  | KSPMsg : KeySignature.Msg,
  | ProbmrMsg : ProbNR.Msg,
  | ProbcrMsg : ProbCR.Msg,
  | ProbOcMelMsg : ProbOctave.Msg,
  | ProbOcChordMsg : ProbOctaveChord.Msg,
  | ProbTypeChordMsg : ProbType.Msg,
  | ProbApChordMsg : ProbApplied.Msg,
  | ProbRootMSgMsg : ProbRoot.Msg
  | ProbAddOn : ProbAddOn.Msg
  | Randomize
  | Default
