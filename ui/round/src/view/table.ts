import { colorName, transWithColorName } from 'common/colorName';
import { MaybeVNodes } from 'common/snabbdom';
import * as game from 'game';
import * as status from 'game/status';
import { isHandicap } from 'shogiops/handicaps';
import { h } from 'snabbdom';
import { renderClock } from '../clock/clockView';
import renderCorresClock from '../corresClock/corresClockView';
import RoundController from '../ctrl';
import { Position } from '../interfaces';
import * as button from './button';
import renderExpiration from './expiration';
import * as replay from './replay';
import * as renderUser from './user';

function renderPlayer(ctrl: RoundController, position: Position) {
  const player = ctrl.playerAt(position);
  return ctrl.nvui
    ? undefined
    : player.ai
      ? h('div.user-link.online.ruser.ruser-' + position, [
          h(`div.player-color.${player.color}`, {
            attrs: {
              title: colorName(
                ctrl.trans.noarg,
                player.color,
                isHandicap({ rules: ctrl.data.game.variant.key, sfen: ctrl.data.game.initialSfen })
              ),
            },
          }),
          h('i.line'),
          h('name', renderUser.aiName(ctrl, player.aiName, player.ai)),
        ])
      : renderUser.userHtml(ctrl, player, position);
}

const isLoading = (ctrl: RoundController): boolean => ctrl.loading || ctrl.redirecting;

const loader = () => h('i.ddloader');

function renderTableWith(ctrl: RoundController, buttons: MaybeVNodes) {
  return [replay.render(ctrl), buttons.find(x => !!x) ? h('div.rcontrols', buttons) : null];
}

export const renderTableEnd = (ctrl: RoundController) => {
  return renderTableWith(ctrl, [isLoading(ctrl) ? loader() : button.backToTournament(ctrl) || button.followUp(ctrl)]);
};

export const renderTableWatch = (ctrl: RoundController) => {
  return renderTableWith(ctrl, [
    isLoading(ctrl) ? loader() : game.playable(ctrl.data) ? undefined : button.watcherFollowUp(ctrl),
  ]);
};

export const renderTablePlay = (ctrl: RoundController) => {
  const d = ctrl.data,
    loading = isLoading(ctrl),
    submit = button.submitUsi(ctrl),
    icons =
      loading || submit
        ? []
        : [
            game.abortable(d)
              ? button.standard(ctrl, undefined, 'L', 'abortGame', 'abort')
              : button.standard(ctrl, game.takebackable, 'i', 'proposeATakeback', 'takeback-yes', ctrl.takebackYes),
            ctrl.data.game.variant.key !== 'chushogi'
              ? button.impasse(ctrl)
              : ctrl.drawConfirm
                ? button.drawConfirm(ctrl)
                : button.standard(ctrl, ctrl.canOfferDraw, '', 'offerDraw', 'draw-yes', () => ctrl.offerDraw(true)),
            ctrl.resignConfirm
              ? button.resignConfirm(ctrl)
              : button.standard(ctrl, game.resignable, 'b', 'resign', 'resign-confirm', () => ctrl.resign(true)),
            replay.analysisButton(ctrl),
          ],
    buttons: MaybeVNodes = loading
      ? [loader()]
      : submit
        ? [submit]
        : [
            button.opponentGone(ctrl),
            button.impasseHelp(ctrl),
            button.cancelDrawOffer(ctrl),
            button.answerOpponentDrawOffer(ctrl),
            button.cancelTakebackProposition(ctrl),
            button.answerOpponentTakebackProposition(ctrl),
          ];
  return [
    replay.render(ctrl),
    h('div.rcontrols', [
      ...buttons,
      h(
        'div.ricons',
        {
          class: {
            confirm: !!(ctrl.drawConfirm || ctrl.resignConfirm),
            empty: icons.length === 0,
          },
        },
        icons
      ),
    ]),
  ];
};

function whosTurn(ctrl: RoundController, color: Color, position: Position) {
  const d = ctrl.data;
  if (status.finished(d) || status.aborted(d)) return;
  return h('div.rclock.rclock-turn.rclock-' + position, [
    d.game.player === color
      ? h(
          'div.rclock-turn__text',
          d.player.spectator
            ? transWithColorName(
                ctrl.trans,
                'xPlays',
                d.game.player,
                isHandicap({ rules: d.game.variant.key, sfen: d.game.initialSfen })
              )
            : ctrl.trans(d.game.player === d.player.color ? 'yourTurn' : 'waitingForOpponent')
        )
      : null,
  ]);
}

function anyClock(ctrl: RoundController, position: Position) {
  const player = ctrl.playerAt(position);
  if (ctrl.clock) return renderClock(ctrl, player, position);
  else if (ctrl.data.correspondence && ctrl.data.game.plies > 1 && !(ctrl.data.game.status.id > 20))
    return renderCorresClock(ctrl.corresClock!, ctrl.trans, player.color, position, ctrl.data.game.player);
  else return whosTurn(ctrl, player.color, position);
}

export const renderTable = (ctrl: RoundController): MaybeVNodes => [
  h('div.round__app__table'),
  renderExpiration(ctrl),
  renderPlayer(ctrl, 'top'),
  ...(ctrl.data.player.spectator
    ? renderTableWatch(ctrl)
    : game.playable(ctrl.data)
      ? renderTablePlay(ctrl)
      : renderTableEnd(ctrl)),
  renderPlayer(ctrl, 'bottom'),
  /* render clocks after players so they display on top of them in col1,
   * since they occupy the same grid cell. This is required to avoid
   * having two columns with min-content, which causes the horizontal moves
   * to overflow: it couldn't be contained in the parent anymore */
  anyClock(ctrl, 'top'),
  anyClock(ctrl, 'bottom'),
];
