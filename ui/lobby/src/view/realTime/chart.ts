import { bind } from 'common/snabbdom';
import { getPerfIcon } from 'common/perfIcons';
import { VNode, h } from 'snabbdom';
import LobbyController from '../../ctrl';
import { Hook } from '../../interfaces';
import { action } from '../../hookRepo';

function percents(v) {
  return v + '%';
}

function ratingLog(a) {
  return Math.log(a / 150 + 1);
}

function ratingY(e?: number) {
  const rating = Math.max(1000, Math.min(2200, e || 1500));
  let ratio;
  let mid = 2 / 5;
  if (rating == 1500) {
    ratio = mid;
  } else if (rating > 1500) {
    ratio = mid + (ratingLog(rating - 1500) / ratingLog(1300)) * 2 * mid;
  } else {
    ratio = mid - (ratingLog(1500 - rating) / ratingLog(500)) * mid;
  }
  return Math.round(ratio * 94);
}

const clockMax = 2000;

function clockX(dur) {
  function durLog(a) {
    return Math.log((a - 30) / 200 + 1);
  }
  return Math.round((durLog(Math.min(clockMax, dur || clockMax)) / durLog(clockMax)) * 100);
}

function renderPlot(ctrl: LobbyController, hook: Hook) {
  const bottom = Math.max(0, ratingY(hook.rating) - 2),
    left = Math.max(0, clockX(hook.t) - 2),
    klass = ['plot.new', hook.ra ? 'rated' : 'casual', action(hook) === 'cancel' ? 'cancel' : ''].join('.');
  return h('span#' + hook.id + '.' + klass, {
    key: hook.id,
    attrs: {
      'data-icon': getPerfIcon(hook.perf || hook.variant || 'standard'),
      style: `bottom:${percents(bottom)};left:${percents(left)}`,
    },
    hook: {
      insert(vnode) {
        $(vnode.elm as HTMLElement)
          .powerTip({
            intentPollInterval: 100,
            placement: hook.rating && hook.rating > 1800 ? 'se' : 'ne',
            mouseOnToPopup: true,
            closeDelay: 200,
            popupId: 'hook',
          })
          .data('powertipjq', $(renderHook(ctrl, hook)))
          .on({
            powerTipRender() {
              $('#hook .inner-clickable').click(() => {
                ctrl.clickHook(hook.id);
              });
            },
          });
        setTimeout(function () {
          (vnode.elm as HTMLElement).classList.remove('new');
        }, 20);
      },
      destroy(vnode) {
        $(vnode.elm as HTMLElement).data('powertipjq', null);
        $.powerTip.destroy(vnode.elm as HTMLElement);
      },
    },
  });
}

function renderHook(ctrl: LobbyController, hook: Hook): string {
  const color = hook.c || 'random';
  let html = '<div class="inner">';
  if (hook.rating) {
    html += '<a class="opponent ulpt is color-icon ' + color + '" href="/@/' + hook.u + '">';
    html += ' ' + hook.u + ' (' + hook.rating + (hook.prov ? '?' : '') + ')';
    html += '</a>';
  } else {
    html += '<span class="opponent anon ' + color + '">' + ctrl.trans('anonymous') + '</span>';
  }
  html += '<div class="inner-clickable">';
  html += `<div>${hook.clock}</div>`;
  html +=
    '<i data-icon="' +
    getPerfIcon(hook.perf || hook.variant || 'standard') +
    '"> ' +
    ctrl.trans(hook.ra ? 'rated' : 'casual') +
    '</i>';
  html += '</div>';
  html += '</div>';
  return html;
}

const xMarks = [1, 2, 3, 5, 7, 10, 15, 20, 30];

function renderXAxis() {
  const tags: VNode[] = [];
  xMarks.forEach(v => {
    const l = clockX(v * 60);
    tags.push(
      h(
        'span.x.label',
        {
          attrs: { style: 'left:' + percents(l - 1.5) },
        },
        '' + v
      )
    );
    tags.push(
      h('div.grid.vert', {
        attrs: { style: 'width:' + percents(l) },
      })
    );
  });
  return tags;
}

const yMarks = [1000, 1200, 1400, 1500, 1600, 1800, 2000];

function renderYAxis() {
  const tags: VNode[] = [];
  yMarks.forEach(function (v) {
    const b = ratingY(v);
    tags.push(
      h(
        'span.y.label',
        {
          attrs: { style: 'bottom:' + percents(b + 1) },
        },
        '' + v
      )
    );
    tags.push(
      h('div.grid.horiz', {
        attrs: { style: 'height:' + percents(b + 0.8) },
      })
    );
  });
  return tags;
}

export function toggle(ctrl: LobbyController) {
  return h('i.toggle', {
    key: 'set-mode-list',
    attrs: { title: ctrl.trans.noarg('list'), 'data-icon': '?' },
    hook: bind('mousedown', _ => ctrl.setMode('list'), ctrl.redraw),
  });
}

export function render(ctrl: LobbyController, hooks: Hook[]) {
  return h('div.hooks__chart', [
    h(
      'div.canvas',
      {
        hook: bind(
          'click',
          e => {
            if ((e.target as HTMLElement).classList.contains('plot')) ctrl.clickHook((e.target as HTMLElement).id);
          },
          ctrl.redraw
        ),
      },
      hooks.map(hook => renderPlot(ctrl, hook))
    ),
    ...renderYAxis(),
    ...renderXAxis(),
  ]);
}
