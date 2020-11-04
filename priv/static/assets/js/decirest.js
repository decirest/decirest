window.decirest_init = function (){

    Vue.component('decirest-object', {
        template: '<ol style="border: 1px solid">' +
        '<template v-for="(value, value_key) in data">' +
        '   <dt>{{value_key}}</dt>' +
        '   <dd v-if="value_key.endsWith(\'_url\')"><a :href="value">{{value}}</a></dd>' +
        //'   <dd v-else-if="value.constructor === Object">' +
        //'       <decirest-object v-bind:data="value"></decirest-object>' +
        //'   </dd>' +
        //'   <dd v-else-if="value.constructor === Array">' +
        //'       <decirest-list v-bind:dataList="value"></decirest-list>' +
        //'   </dd>' +
        '   <dd v-else>{{value}}</dd>' +
        '</template></ol>',
        props: ['data']

    });
    /*
    Vue.component('decirest-list-item', {
        template: '<decirest-object v-if="item.constructor === Object" v-bind:data="item"></decirest-object>' +
        '<decirest-list v-if="item.constructor === Array" v-bind:dataList="item"></decirest-list>' +
        '<span>{{item}}</span>',
        props: ['item']
    })
    Vue.component('decirest-list', {
        template: '<div class="list">' +
        '<decirest-list-item v-for="item in dataList" v-bind:item="item"></decirest-list-item>' +
        '</div>',
        props: ['dataList']
    });
    */
    Vue.component('decirest-collection-component', {
        template: '<div class="data-collection">' +
        //'<decirest-list v-bind:dataList = "collection"></decirest-list>' +
        '<decirest-object v-for="item in collection" v-bind:data="item" :key="item[pk]"></decirest-object>' +
        '</div>',
        props: ['collection', 'pk']
    });
    Vue.component('decirest-single-component', {
        template: '<div class="data-single">' +
        '<decirest-object v-bind:data="single"></decirest-object>' +
        '</div>',
        props: ['single']
    });
    var data_app = new Vue({
        el: '#data_container',
        data: {
            collection: window.decirest_collection_data,
            single: window.decirest_single_data,
            pk: window.decirest_data_pk
        }
    });


    Vue.component('breadcrumb-component',{
        template: '<ul class="breadcrumb">' +
            '<li v-for="p in paths">' +
            '<a :href="p.href">{{p.name}}</a>' +
            '</li>' +
            '<a :href="doc_path.href" style="float: right;">{{doc_path.name}}</a>' +
        '</ul>',
        props: ['paths', 'doc_path']
    });


    var paths = location.pathname.split('/');
    var breadcrumbs = [];
    for (var i = 2; i <= paths.length - 1; i++){
        breadcrumbs.push({name: paths[i], href: paths.slice(0,i + 1).join('/')})
    }
    var doc_url = paths.slice(1, paths.length).join('/');
    var nav_app = new Vue({
        el: '#nav',
        data: {
            paths: breadcrumbs,
            doc_path: {href: "/apidoc/path/"+doc_url, name: "Documentation"}
        }
    })
};
window.decirest_init();
