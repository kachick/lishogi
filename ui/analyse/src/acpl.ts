import { defined } from 'common/common';
import { bind, dataIcon } from 'common/snabbdom';
import * as game from 'game';
import { VNode, VNodeData, h, thunk } from 'snabbdom';
import AnalyseCtrl from './ctrl';
import { findTag } from './study/studyChapters';

type AdviceKind = 'inaccuracy' | 'mistake' | 'blunder';

interface Advice {
  kind: AdviceKind;
  plural: I18nKey;
  symbol: string;
}

function renderRatingDiff(rd: number | undefined): VNode | undefined {
  if (rd === 0) return h('span', '±0');
  if (rd && rd > 0) return h('good', '+' + rd);
  if (rd && rd < 0) return h('bad', '−' + -rd);
  return;
}

function renderPlayer(ctrl: AnalyseCtrl, color: Color): VNode {
  const p = game.getPlayer(ctrl.data, color);
  if (p.user)
    return h(
      'a.user-link.ulpt',
      {
        attrs: { href: '/@/' + p.user.username },
      },
      [p.user.username, ' ', renderRatingDiff(p.ratingDiff)]
    );
  return h(
    'span',
    p.name ||
      (ctrl.data.game.variant.key == 'standard' && p.ai && 'YaneuraOu level ' + p.ai) ||
      (p.ai && 'Fairy-Stockfish level ' + p.ai) ||
      (ctrl.study && findTag(ctrl.study.data.chapter.tags, color)) ||
      'Anonymous'
  );
}

const advices: Advice[] = [
  { kind: 'inaccuracy', plural: 'inaccuracies', symbol: '?!' },
  { kind: 'mistake', plural: 'mistakes', symbol: '?' },
  { kind: 'blunder', plural: 'blunders', symbol: '??' },
];

function playerTable(ctrl: AnalyseCtrl, color: Color): VNode {
  const d = ctrl.data,
    trans = ctrl.trans.noarg;
  const acpl = d.analysis![color].acpl;
  return h(
    'table',
    {
      hook: {
        insert(vnode) {
          window.lishogi.powertip.manualUserIn(vnode.elm);
        },
      },
    },
    [
      h('thead', h('tr', [h('td', h('i.is.color-icon.' + color)), h('th', renderPlayer(ctrl, color))])),
      h(
        'tbody',
        advices
          .map(a => {
            const nb: number = d.analysis![color][a.kind],
              style = nb ? `.symbol.${a.kind}` : '',
              attrs: VNodeData = nb
                ? {
                    'data-color': color,
                    'data-symbol': a.symbol,
                  }
                : {};
            return h('tr' + (nb ? `.symbol${style}` : ''), { attrs }, [h('td', '' + nb), h('th', trans(a.plural))]);
          })
          .concat(h('tr', [h('td', '' + (defined(acpl) ? acpl : '?')), h('th', trans('averageCentipawnLoss'))]))
      ),
    ]
  );
}

function doRender(ctrl: AnalyseCtrl): VNode {
  return h(
    'div.advice-summary',
    {
      hook: {
        insert: vnode => {
          $(vnode.elm as HTMLElement).on('click', 'tr.symbol', function (this: Element) {
            ctrl.jumpToGlyphSymbol($(this).data('color'), $(this).data('symbol'));
          });
        },
      },
    },
    [
      playerTable(ctrl, 'sente'),
      h('div.hidden', '-'),
      ctrl.study
        ? null
        : h(
            'a.button.text',
            {
              class: { active: !!ctrl.retro },
              attrs: dataIcon('G'),
              hook: bind('click', ctrl.toggleRetro, ctrl.redraw),
            },
            ctrl.trans.noarg('learnFromYourMistakes')
          ),
      playerTable(ctrl, 'gote'),
    ]
  );
}

export function render(ctrl: AnalyseCtrl): VNode | undefined {
  if (ctrl.studyPractice || ctrl.embed) return;

  if (!ctrl.data.analysis || !ctrl.showComputer() || (ctrl.study && ctrl.study.vm.toolTab() !== 'serverEval'))
    return h('div.analyse__acpl');

  // don't cache until the analysis is complete!
  const buster = ctrl.data.analysis.partial ? Math.random() : '';
  let cacheKey = '' + buster + !!ctrl.retro;
  if (ctrl.study) cacheKey += ctrl.study.data.chapter.id;

  return h('div.analyse__acpl', thunk('div.advice-summary', doRender, [ctrl, cacheKey]));
}
