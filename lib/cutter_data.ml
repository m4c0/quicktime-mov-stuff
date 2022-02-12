type edit = { dur: int; mtime: int; mrate: float }
type header = { dur: int; scale: int }
type track = { dur: int; media: header; edits: edit list list }
type movie = {
  mvhd : header;
  traks : track list
}

