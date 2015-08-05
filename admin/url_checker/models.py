from django.db import models

class Url(models.Model):
    name = models.CharField(max_length=200)
    url = models.URLField()
