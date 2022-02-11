type header = { dur: int; scale: int }
type track = { dur: int; media: header }
type movie = {
  mvhd : header;
  traks : track list
}

