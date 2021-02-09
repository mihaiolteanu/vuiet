![melpa](https://melpa.org/packages/vuiet-badge.svg)

# Vuiet: music player and explorer

## Music Browser ([details](#browser))

Browse artists, genres or albums with Emacs,
```emacs-lisp
(vuiet-tag-info "progressive rock")
(vuiet-album-info "steven wilson" "to the bone")
(vuiet-artist-info "steven wilson")
...
``` 

![image](https://user-images.githubusercontent.com/8273519/107241018-3c270a80-6a33-11eb-9fda-7213a617be2c.png)

## Music Player ([details](#playlists))

Play an artist top songs, sequentially or randomly,
```emacs-lisp
(vuiet-play-artist '("steven wilson"))
```

or tracks from similar artists, whole albums, loved tracks, or whole genres,
```emacs-lisp
(vuiet-play-artist-similar '("lost in kiev"))
(vuiet-play-album "anathema" "one last goodbye")
(vuiet-play-tag-similar '("classic rock" "80s" "progressive"))
...
```

## Play by Lyrics

Display the lyrics and search the saved lyrics database for a song to play,
```emacs-lisp
(vuiet-play-track-by-lyrics "secret")
```
![image](https://user-images.githubusercontent.com/8273519/74035303-b527f800-49c2-11ea-8a32-15087b2f9e3e.png)

## Other Features

- Display the currently playing song in the mode-line,

![mode-line](https://user-images.githubusercontent.com/8273519/107336357-ac7c6d00-6ac1-11eb-9789-064c1ccd5c1b.png)

- Seek forward, backward, replay, play/pause, open currently playing song on
youtube ([details](#player-interaction)),
```emacs-lisp
(vuiet-seek-forward 15)
(vuiet-playing-track-continue-on-youtube)
...
```

- Scrobble songs to your last.fm profile and add songs to your list of last.fm
  loved songs.

- Search interactively for songs, artists or even albums (fill out part of the
artist name -> TAB -> select the artist -> select the album -> album is now
playing)

![call vuiet-play-album](https://user-images.githubusercontent.com/8273519/107335138-3deadf80-6ac0-11eb-9c87-a820b166b813.png)
![give artist name](https://user-images.githubusercontent.com/8273519/107335144-3e837600-6ac0-11eb-93de-9a48d528c03a.png)
![tab for similar
artists](https://user-images.githubusercontent.com/8273519/107335145-3e837600-6ac0-11eb-8c4e-506ddf9ee421.png)
![select album](https://user-images.githubusercontent.com/8273519/107335147-3f1c0c80-6ac0-11eb-85b2-14daeb9ad118.png)

# Installation Requirements

- Install vuiet from [melpa](https://melpa.org/#/vuiet)
- [youtube-dl](http://ytdl-org.github.io/youtube-dl/download.html),

Install and update youtube-dl with these commands if vuiet suddenly stops working as
youtube changes its algorithms frequently to prevent third party users (us) from using it.
```bash
sudo curl -L https://yt-dl.org/downloads/latest/youtube-dl -o /usr/local/bin/youtube-dl
sudo chmod a+rx /usr/local/bin/youtube-dl
```
- [lastfm](https://last.fm) account, API key, and the
  [lastfm.el](https://github.com/mihaiolteanu/lastfm.el) package (follow the
  instruction on the README page)
- [mpv](https://mpv.io/)

# Complete functionality

## Playlists

**vuiet-play** *songs (random nil)*

    Play everyting in the SONGS list, randomly or sequentially.
    SONGS is a list of type ((artist1 song1) (artist2 song2) ...).

**vuiet-play-artist** *artist random*

    Play the ARTIST top tracks, RANDOM or sequentially.

**vuiet-play-playing-artist** *random*

    Play the currently playing artist's top tracks.

**vuiet-play-playing-track-album**

    Play the full album of the currently playing track.

**vuiet-info-playing-track-album**

    Open an info buffer for the currently playing track album.
    
**vuiet-play-album** *artist album*

    Play the whole ALBUM of the given ARTIST.
    If called interactively, the album can be picked interactively
    from the ARTIST's top albums.

**vuiet-play-artist-similar** *artists*

    Play tracks from artists similar to ARTISTS.
    ARTISTS is a list of strings of the form '(artist1 artist2 etc.)
    If called interactively, multiple artists can be provided in the
    minibuffer if they are sepparated by commas.
  
**vuiet-play-playing-artist-similar**

    Play tracks from artists similar to the playing artist.
    This function is similar to `vuiet-play-artist-similar', only the
    list of artists is limited to the artist of the currently playing
    track.

**vuiet-play-tag-similar** *tags*

    Play tracks from artists similar to TAGS.
    TAGS is a list of strings of the form '(tag1 tag2 etc.)
    If called interactively, multiple tags can be provided in the
    minibuffer if they are sepparated by commas.
    
**vuiet-play-playing-tags-similar**

    Play tracks from artists with similar tags as the current tags.
    Play tracks from random artists that have tags equal to one of
    the tags of the currently playing artist.

**vuiet-play-track** *artist name*

    Play the song NAME from the given ARTIST.
    If called interactively, let the user select and play one of the
    ARTIST's top songs, where ARTIST is given in the minibuffer.

**vuiet-play-track-search** *track*

    Search TRACK and play the selected item.
    Similar to `vuiet-play-track', but search for TRACK on last.fm
    first and then let the user select one of the results.

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

**vuiet-play-artist-loved-tracks** *artist random*

    Play all the ARTIST tracks found in the user loved tracks.
    Similar to `vuiet-play-loved-tracks', but play only the tracks
    from the given ARTIST.

**vuiet-play-recent-track**

    Play one of the recent listened tracks.

**vuiet-play-loved-tracks-similar**

    Play tracks based on artists similar to loved tracks artists.
    Play tracks from random artists similar to a random artist from
    the list of user loved tracks.

## Browser

**vuiet-artist-info** *artist*

    Display info about ARTIST in a new buffer.

    p   play all the artist songs, sequentially.
    s   select and display info for a similar artist with ivy.
    l   visit the artist's lastfm page.

**vuiet-artist-info-search** *artist*

    Search ARTIST and display info about the selected item.
    Similar to `vuiet-artist-info', but search for ARTIST on last.fm
    first and then display the info about it.

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
    
**vuiet-peek-next**

    Display the next track in the mode-line for a few seconds.
    
**vuiet-previous**

    Replay the previous track.
    
**vuiet-replay**

    Play the currently playing track from the beginning.
    
**vuiet-seek-backward** *(arg)*

    Seek backward the given number of ARG.  ARG defaults to 5 seconds.

**vuiet-seek-forward** *(arg)*

    Seek forward the given number of ARG.  ARG defaults to 5 seconds.

**vuiet-seek-backward-rate** *(arg)*

    Seek backward ARG% of the track.  ARG defaults to 10%.

**vuiet-seek-forward-rate** *(arg)*

    Seek forward ARG% of the track.  ARG defaults to 10%.

**vuiet-play-pause**

    Toggle the play/pause status.
    
**vuiet-player-volume**

    Get the music player volume, between 0% and 100%.
    
**vuiet-player-volume-inc** *(arg)*

    Increase the music player volume by ARG percent.  ARG defaults to 10%.

**vuiet-player-volume-dec** *(arg)*
    
    Decrease the music player volume by ARG percent.  ARG defaults to 10%.
    
**vuiet-playing-artist-info**

    Display info for the currently playing artist in a new buffer.
    
**vuiet-playing-track-search-youtube**

    Open a youtube search for the currently playing track.
    
**vuiet-playing-track-continue-on-youtube**

    Pause vuiet and continue playing on youtube.

**vuiet-playing-track-continue-with-mpv**

    Pause vuiet and continue playing with mpv as a new process.
    
**vuiet-artist-lastfm-page** *artist*
    
    Visit the ARTIST lastfm page."

**vuiet-playing-artist-lastfm-page**

    Visit he currently playing artist lastfm page.
    
**vuiet-love-track**

    Add the currently playing track to the user loved songs.
    
**vuiet-unlove-track**

    Remove the currently playing track from the user loved songs.
    
**vuiet-playing-track-lyrics**

    Display the lyrics for the currently playing track in a new buffer.
    See `versuri-display' for the active keybindings inside this buffer.
                    
**vuiet-update-mode-line**

    Update the mode line.
    
## Customization

**vuiet-scrobble-timeout** *30*

    Time, in seconds, for the same song to play before scrobbling it.
    
**vuiet-scrobble-enabled** *t*

    Enable/disable last.fm scrobbling.
    Decide if the currently playing track should appear in your list
    of recently played tracks on last.fm.

**vuiet-update-mode-line-automatically** t

    Enable/disable the automatic update of the mode-line.
    If enabled, the mode-line is automatically updated after
    `vuiet-update-mode-line-interval' seconds. More specifically,
    `vuiet-update-mode-line' is called periodically while a track is
    playing to update it's current playback position.

**vuiet-update-mode-line-interval** 10

    Timeout, in seconds, after which to update the mode-line.
    See the `vuiet-update-mode-line-automatically' custom variable
    for details.

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
    value increases the chances you'll discover something totally new
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
