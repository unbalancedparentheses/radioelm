from django.contrib import admin
from django.utils.html import format_html

from .models import Url

class UrlAdmin(admin.ModelAdmin):
    fields = ['name', 'url']
    list_display = ['name', 'show_url']


    def show_url(self, obj):
        return format_html("<a href='{url}'>{url}</a>", url=obj.url)

    show_url.allow_tags = True

admin.site.register(Url, UrlAdmin)
