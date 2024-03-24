namespace Type

type MusicGenre =
  | House of string
  | Funk of string
  | HipHop of string

type Location = Location of string

type PeriodOfYears = { start: int; endValue: int}

type YearsActive =
  | StillActive of since: int * previousPeriods: List<PeriodOfYears>
  | ActiveInPast of List<PeriodOfYears>

type SearchByGenre = { genres: List<MusicGenre>}

type SearchByOrigin = { locations: List<Location>}
type SearchByActiveYears = { start: int; endValue: int}
type SearchByActiveLength = { howLong: int; until: int}

type SearchCondition =
  | SearchByGenre of SearchByGenre
  | SearchByOrigin of SearchByOrigin
  | SearchByActiveYears of SearchByActiveYears
  | SearchByActiveLength of SearchByActiveLength

type Artist ={
  name: string;
  genre: MusicGenre;
  origin: Location;
  yearsActive: YearsActive;
}

module ArtistModule =
  let activeLength (artist: Artist) (until: int): int =
    let periods =
      match artist.yearsActive with
      | StillActive (since, previousPeriods) -> previousPeriods @ [{ start = since; endValue = until }]
      | ActiveInPast periods -> periods
    periods |> List.map (fun period -> period.endValue - period.start) |> List.fold (fun x y -> x + y) 0

  let PeriodOverlapWithPeriods(checkedPeriod: PeriodOfYears) (periods: List<PeriodOfYears>): bool =
    periods
    |> List.exists (fun period -> period.start <= checkedPeriod.endValue && period.endValue >= checkedPeriod.start)

  let wasArtistActive (artist: Artist) (searchedPeriod: PeriodOfYears): bool =
    match artist.yearsActive with
      | StillActive (since=since; previousPeriods=periods) -> since <= searchedPeriod.endValue || PeriodOverlapWithPeriods searchedPeriod periods
      | ActiveInPast periods -> PeriodOverlapWithPeriods searchedPeriod periods
  let searchArtist (artists: List<Artist>) (requiredConditions: List<SearchCondition>): List<Artist> =
    artists
    |> List.filter (fun artist ->
      requiredConditions
      |> List.forall (fun condition ->
        match condition with
          | SearchByGenre { genres = genres } -> genres |> List.exists (fun genre -> genre = artist.genre)
          | SearchByOrigin { locations = locations } -> locations |> List.exists (fun location -> location = artist.origin)
          | SearchByActiveYears ({ start = start; endValue = endValue }) -> wasArtistActive artist {start=start; endValue=endValue}
          | SearchByActiveLength { howLong = howLong; until = until } -> activeLength(artist) (until) >= howLong
      ))