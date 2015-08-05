# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations

def starting_urls(apps, schema_editor):
    Url = apps.get_model("url_checker", "url")

    Url(name="mitre", url="http://buecrplb01.cienradios.com.ar/Mitre790.aac").save()
    Url(name="continental", url="http://1351.live.streamtheworld.com:80/CONTINENTAL_SC").save()
    Url(name="radio argentina", url="http://wmserver3.aginet.com.ar:13574/;stream/1/").save()
    Url(name="los 40", url="http://5133.live.streamtheworld.com:80/LOS40_ARGENTINA_SC").save()
    Url(name="la 100", url="http://buecrplb01.cienradios.com.ar/la100.aac").save()
    Url(name="espn", url="http://edge.espn.cdn.abacast.net/espn-deportesmp3-48").save()
    Url(name="imagina", url="http://7309.live.streamtheworld.com:80/IMAGINA_ARGENTINA_SC").save()
    Url(name="nova", url="http://buecrplb01.cienradios.com.ar/fm979.mp3").save()
    Url(name="sonic", url="http://live.chicago.cdn.sonic.fm:8000/live128").save()
    Url(name="el mundo", url="http://radiostream.elmundoradio.com:8332/;").save()

class Migration(migrations.Migration):

    dependencies = [
    ]

    operations = [
        migrations.CreateModel(
            name='Url',
            fields=[
                ('id', models.AutoField(primary_key=True, serialize=False, auto_created=True, verbose_name='ID')),
                ('name', models.CharField(max_length=200)),
                ('url', models.URLField()),
            ],
        ),

        migrations.RunPython(starting_urls),
    ]
