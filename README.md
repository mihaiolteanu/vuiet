# Vuiet, the music player and explorer for Emacs

Vuiet is a music player based on similarities between artists and genres. Vuiet
takes advantage of lastfm's huge music dataset to figure out artists top tracks,
genres and the relations between them.

Vuiet searches all the tracks on youtube and plays them in the background with
mpv. Do some keybindings for play, pause and next-track, among others, and you have a
hassle free music player.

You could say this is something similar to lastfm or spotify but with more user
control.

All in all, vuiet is a great way to discover new music without leaving the
comfort of your keyboard.

## Music player

You can ask vuiet to play random tracks from random artists similar to an artist
you already know and like. For example,

```emacs-lisp
(vuiet-play-artist-similar '("steven wilson"))
```

There is no limit to the number of artists specified in this call. New tracks
will be selected from random artists similar to artists from this list. The
number of similar artists and the number of top tracks to consider, among
others, is fully [customizable](#customization).

Or you can play random tracks from random artists that are tagged with any of
the genres you specify,

```emacs-lisp
(vuiet-play-tag-similar '("classic rock" "80s"))
```

There are multiple variations on these two type of playlists, some of them
random and basically infinite, others finite and sequential, some based on
artists, other on genres or on lastfm user loved tracks or even single track play,
given directly or from a lyrics search. The posibilities are endless. See the
[playlists](#playlists) section for details.

## Music browser

Besides the player, there is also an explorer. I won't bore you with the
details, just know that you can call it, and then explore everything on that
page. Handy for picking out specific tracks for listening or for casually
browsing similar artists or genres.
 
```emacs-lisp
(vuiet-artist-info "steven wilson")
``` 

![image](https://user-images.githubusercontent.com/8273519/74035278-a80b0900-49c2-11ea-90db-7602a5eef2fc.png)

## Search, browse and play from lyrics

Google is nice for searching that song that's in your head. But sometimes you
just can't find it. Either the lyrics are too general and Google finds only
cr**, either your track is obscure enogh that nobody has heard about it.

Either way, you can save the lyrics for each song you play with `vuiet`. All
your saved lyrics are searchable, filterable and playable. Having a Google-like
search for your local database of lyrics can come in handy.

Here, let me show you,

```emacs-lisp
(vuiet-play-track-by-lyrics "secret")
```

![image](https://user-images.githubusercontent.com/8273519/74035303-b527f800-49c2-11ea-8a32-15087b2f9e3e.png)

`Vuiet` searched through all the lyrics in my database for the ones that contain
`secret` in them.  Hit `<enter>` on any song from that list, and `vuiet` will
play it. Nice, eh?

# Requirements

The recommendation system is based on [lastfm](https://last.fm). So you should
have a lastfm account and the
[lastfm.el](https://github.com/mihaiolteanu/lastfm.el) package installed
first (see the lastfm.el README for details). The tracks themselves are taken from
youtube via [youtube-dl](https://github.com/ytdl-org/youtube-dl/) and played
with [mpv](https://mpv.io/) in the background (i.e. with the --no-video
option). You should have both of them installed.

# Complete functionality

## Playlists

**vuiet-play-artist** *artist random*

    Play the ARTIST top tracks.
    If RANDOM is t, play the tracks at random, indefinitely.
    The number of tracks is equal to VUIET-ARTIST-TRACKS-LIMIT.

**vuiet-play-playing-artist** *random*

    Play the currently playing artist's top tracks.
    If RANDOM is t, play the tracks at random, indefinitely.
    The number of tracks is equal to VUIET-ARTIST-TRACKS-LIMIT.

**vuiet-play-artist-similar** *artists*

    Play tracks from artists similar to ARTISTS.
    Random tracks from random artists similar to one of the ARTISTS
    are played.
    The number of similar artists taken into account is equal to
    VUIET-ARTIST-SIMILAR-LIMIT and the number of tracks is equal to
    VUIET-ARTIST-TRACKS-LIMIT.
    
**vuiet-play-playing-artist-similar**

    Play tracks from artists similar to the playing artist.
    Random tracks from random artists similar to the currently
    playing artist are played.
    The number of similar artists taken into account is equal to
    VUIET-ARTIST-SIMILAR-LIMIT and the number of tracks is equal to
    VUIET-ARTIST-TRACKS-LIMIT.

**vuiet-play-tag-similar** *tags*

    Play tracks from artists similar to TAGS.
    Random tracks from random artists that have tags equal to one of
    the TAGS are played.
    The number of artists with the given tag taken into account is
    equal to VUIET-TAG-ARTISTS-LIMIT while the number of tracks is
    equal to VUIET-ARTIST-TRACKS-LIMIT.
    
**vuiet-play-playing-tags-similar**

    Play tracks from artists with similar tags as the current tags.
    Random tracks from random artists that have tags equal to one of
    the tags of the currently playing artist are played.
    The number of artists with the given tag taken into account is
    equal to VUIET-TAG-ARTISTS-LIMIT while the number of tracks is
    equal to VUIET-ARTIST-TRACKS-LIMIT.

**vuiet-play-track** *artist name*

    Play track NAME from ARTIST.

**vuiet-play-track-search** *track*

    Search TRACK and play the selected item.
    Similar to `vuiet-play-track', but search for TRACK on last.fm
    first and then let the user select one of the results.  The
    selected item is what is played by vuiet.  Useful if you don't
    know the exact name and/or artist of the song.

**vuiet-play-track-by-lyrics** *lyrics*

    Search a track by LYRICS and play it.
    
**vuiet-play-loved-track**

    Select a track from the user loved tracks and play it.
    The user loved tracks list is the one associated with the
    username given in the setup of the lastfm.el package.
    
**vuiet-play-loved-tracks** *(random nil)*

    Play all the tracks from the user loved tracks.
    If RANDOM is t, play the tracks at random, indefinitely.
    The user loved tracks list is the one associated with the
    username given in the setup of the lastfm.el package.

**vuiet-play-recent-track**

    Play one of the recent listened tracks.

**vuiet-play-loved-tracks-similar**

    Play tracks based on artists similar to loved tracks artists.
    Random tracks from random artists similar to a random artist from
    the list of user loved tracks are played.
    The user loved tracks list is the one associated with the
    username given in the setup of the lastfm.el package.
    The number of similar artists taken into account is equal to
    VUIET-ARTIST-SIMILAR-LIMIT and the number of tracks is equal to
    VUIET-ARTIST-TRACKS-LIMIT.
    
**vuiet-pick-album** *artist*

    Display an album from ARTIST and optionally play it.
    The album is displayed in a dedicated buffer.  See
    `vuiet-album-info' for details and active keybindings inside this
    buffer.

**vuiet-play** *item (random nil)*

    Play the ITEM with mpv and scrobble to lastfm.
    RANDOM is used only if the ITEM list is not already a generator.

    If ITEM is a VUIET-TRACK object, play it.

    If ITEM is a (ARTIST SONG) form, where ARTIST and SONG are
    strings, create a VUIET-TRACK object and call this function again
    with this object.

    If ITEM is a list of (ARTIST SONG) forms, create a generator of
    VUIET-TRACK objects and call VUIET-PLAY again with the generator.

    If ITEM is a generator, play the next VUIET-TRACK object from
    that generator and set an mpv hook on exit.  When the hook is
    called (mvp exists, track finished playing) call VUIET-PLAY again
    with the same generator.

## Music Browser

**vuiet-artist-info** *artist*

    Display info about ARTIST in a new buffer.

    p   play all the artist songs, sequentially.
    s   select and display info for a similar artist with ivy.
    l   visit the artist's lastfm page.

**vuiet-artist-info-search** *artist*

    Search ARTIST and display info about the selected item.
    Similar to `vuiet-artist-info', but search for ARTIST on last.fm
    first and then let the user select one artist from the resulting
    list of artists.  Vuiet then displays the info about the user
    selected artist.  Useful if you don't know the exact name of the
    artist.

**vuiet-tag-info** *tag*

    Display info about TAG in a new buffer.

**vuiet-loved-tracks-info** *(page 1) (n 50)*

    Display N tracks from the user loved tracks in a new buffer.
    If the user has more than N loved tracks, PAGE can be used to show
    the next PAGE * N tracks.

    <enter>  On a song entry, plays that song only.
    i        Display the next PAGE * N songs.
    u        Display the previous PAGE * N songs, if N > 1
    s        Choose a song to play, with ivy.

**vuiet-album-info** *artist album*

    Display info about the ARTIST's ALBUM in a new buffer.

    s   choose a song with ivy.
    a   pick another album with ivy.
    p   play all songs from the album.
    l   save lyrics for this album.

**vuiet-album-info-search** *artist*

    Search all albums from ARTIST and display the selected one.
    The album is displayed in a dedicated buffer.  See
    `vuiet-album-info' for details regarding the active keybindings
    inside this buffer.

## Player Interaction

**vuiet-stop** 

    Stop playing and clear the mode line.
    
**vuiet-playing-artist** 

    Return the currently playing artist.
    
**vuiet-playing-track-name** 

    Return the currently playing track name.
    
**vuiet-playing-track-str** 

    Return the playing TRACK as a human-readable string.
    
**vuiet-next** 

    Skip the currently playing track and play the next.
    
**vuiet-replay**

    Play the currently playing track from the beginning.
    
**vuiet-play-pause**

    Toggle the play/pause status.
    
**vuiet-playing-artist-info**

    Display info for the currently playing artist in a new buffer.
    
**vuiet-playing-track-search-youtube**

    Open a youtube search for the currently playing track.
    
**vuiet-artist-lastfm-page** *artist*
    
    Visit the ARTIST lastfm page."

**vuiet-playing-artist-lastfm-page**

    Visit he currently playing artist lastfm page.
    
**vuiet-love-track**

    Add the currently playing track to the loved songs list.
    
**vuiet-unlove-track**

    Remove the currently playing track from the loved songs list.
    
**vuiet-playing-track-lyrics**

    Display the lyrics for the currently playing track in a new buffer.
    See `versuri-display' for the active keybindings inside this buffer.
           
## Customization

**vuiet-scrobble-timeout** *30*

    Time, in seconds, for the same song to play before scrobbling it.
    A gigantic value basically disables scrobbling altogether.

**vuiet-artist-similar-limit** *15*

    Number of artists similar to the given artist.
    When considering artists similar to a given artist, take as many
    into consideration as this limit.  A lower value might mean
    artists and tracks you already know and love.  A higher value
    increases the chances you'll discover something totally new.

**vuiet-artist-tracks-limit** *15*

    Number of tracks for the given artist.
    When considering the top tracks for a given artist, take as many
    into consideration as this limit.  A lower value might mean
    tracks from this artist that you already know and love.  A higher
    value increases the changes you'll discover something totally new
    but it also increases the chances that you'll get wrongly
    scrobbled songs and youtube will find something totally unrelated
    as a result.

**vuiet-tag-artists-limit** *15*

    Number of artists for the given tag.
    When considering the top artists for a given tag, take as many
    into consideration as this limit.

**vuiet-loved-tracks-limit** *500*

    Number of tracks to take into consideration when playing user loved tracks.
    A number higher than your actual lastfm loved tracks, will take
    all of them into consideration.  A lower values is useful for
    taking into consideration only the most recently loved tracks.
