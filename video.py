import youtube_dl

def download_subs(url, lang="en"):
    opts = {
        "skip_download": True,
        "writesubtitles": "%(name)s.vtt",
        "writeautomaticsub": "%(name)s_auto.vtt",
        "subtitlelangs": lang,
        "format": "worst"
    }

    with youtube_dl.YoutubeDL(opts) as yt:
        yt.download([url])

url = 'https://www.youtube.com/watch?v=rudHe4zdwUc'
download_subs(url)
